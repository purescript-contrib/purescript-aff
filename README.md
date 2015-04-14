# purescript-aff

An asynchronous effect monad for PureScript.

The moral equivalent of `ErrorT (ContT Unit (Eff e) a`, for effects `e`.

`Aff` lets you say goodbye to monad transformers and callback hell!

# Example

```purescript
main = launchAff $ 
  do response <- Ajax.get "http://foo.bar"
     liftEff $ trace response.body
```

See the [examples directory](/examples/src/Examples.purs) for more examples.

# Getting Started

## Installation

```
bower install purescript-aff
```

## Introduction

An example of `Aff` is shown below:

```purescript
deleteBlankLines path =
  do  contents <- loadFile path
      let contents' = S.join "\n" $ A.filter (\a -> S.length a > 0) (S.split "\n" contents)
      saveFile path contents'
```

This looks like ordinary, synchronous, imperative code, but actually operates asynchronously without any callbacks. Error handling is baked in so you only deal with it when you want to.

The library contains instances for `Semigroup`, `Monoid`, `Apply`, `Applicative`, `Bind`, `Monad`, `Alt`, `Plus`, `MonadPlus`, `MonadEff`, and `MonadError`. These instances allow you to compose asynchronous code as easily as `Eff`, as well as interop with existing `Eff` code.

## Escaping Callback Hell

Hopefully, you're using libraries that already use the `Aff` type, so you don't even have to think about callbacks!

If you're building your own library, or you have to interact with some native code that expects callbacks, then *purescript-aff* provides a `makeAff` function:

```purescript
makeAff :: forall e a. ((Error -> Eff e Unit) -> (a -> Eff e Unit) -> Eff e Unit) -> Aff e a
```

This function expects you to provide a handler, which should call a user-supplied error callback or success callback with the result of the asynchronous computation.

For example, let's say we have an AJAX request function that expects a callback:

```purescript
foreign import ajaxGet """
function ajaxGet(callback) { // accepts a callback
  return function(request) { // and a request
    return function() { // returns an effect
      doNativeRequest(request, function(response) {
        callback(response)(); // callback itself returns an effect
      });
    }
  }
}
""" :: forall e. (Response -> Eff e Unit) -> Request -> Eff e Unit
```

We can wrap this into an asynchronous computation like so:

```purescript
ajaxGet' :: forall e. Request -> Aff e Response
ajaxGet' req = makeAff (\error success -> ajaxGet success req)
```

This eliminates callback hell and allows us to write code simply using `do` notation:

```purescript
do response <- ajaxGet' req
   liftEff $ trace response.body
```

## Eff

All purely synchronous computations (`Eff`) can be lifted to asynchronous computations with `liftEff` defined in `Control.Monad.Eff.Class` (see [here](https://github.com/paf31/purescript-monad-eff)).

```purescript
import Control.Monad.Eff.Class

liftEff $ trace "Hello world!"
```

This lets you write your whole program in `Aff`, and still call out to synchronous code.

If your `Eff` code throws exceptions (`err :: Exception`), you can remove the exceptions using `liftEff'`, which brings exceptions to the value level as an `Either Error a`:

```purescript
do e <- liftEff' myExcFunc
   liftEff $ either (const $ trace "Oh noes!") (const $ trace "Yays!") e
```

## Dealing with Failure

The `Aff` monad has error handling baked in, so ordinarily you don't have to worry about it.

When you need to deal with failure, you have several options.

 1. **Attempt**
 2. **Alt**
 3. **MonadError**

#### 1. Attempt

If you want to attempt a computation but recover from failure, you can use the `attempt` function:

```purescript
attempt :: forall e a. Aff e a -> Aff e (Either Error a)
```

This returns an `Either Error a` that you can use to recover from failure.

```purescript
do e <- attempt $ Ajax.get "http://foo.com"
   liftEff $ either (const $ trace "Oh noes!") (const $ trace "Yays!") e
```

#### 2. Alt

Because `Aff` has an `Alt` instance, you may also use the operator `<|>` to provide an alternative computation in the event of failure:

```purescript
do result <- Ajax.get "http://foo.com" <|> Ajax.get "http://bar.com"
   return result
```

#### 3. MonadError

`Aff` has a `MonadError` instance, which comes with two functions: `catchError`, and `throwError`.

These are defined in [purescript-transformers](http://github.com/purescript/purescript-transformers).
Here's an example of how you can use them:

```purescript
do resp <- (Ajax.get "http://foo.com") `catchError` (const $ pure defaultResponse)
   if resp.statusCode != 200 then throwError myErr 
   else pure resp.body
```

Thrown exceptions are propagated on the error channel, and can be recovered from using `attempt` or `catchError`.

## Forking

Using the `forkAff`, you can "fork" an asynchronous computation, which means 
that its activities will not block the current thread of execution:

```purescript
forkAff myAff
```

Because Javascript is single-threaded, forking does not actually cause the 
computation to be run in a separate thread. Forking just allows the subsequent 
actions to execute without waiting for the forked computation to complete.

If the asynchronous computation supports it, you can "kill" a forked computation
using the returned canceler:

```purescript
canceler <- forkAff myAff
canceled <- canceler `cancel` (error "Just had to cancel")
_        <- liftEff $ if canceled then (trace "Canceled") else (trace "Not Canceled")
```

If you want to run a custom canceler if some other asynchronous computation is
cancelled, you can use the `cancelWith` combinator:

```purescript
otherAff `cancelWith` myCanceler
```

## AVars

The `Control.Monad.Aff.AVar` module contains asynchronous variables, which are very similar to Haskell's `MVar` construct. These can be used as low-level building blocks for asynchronous programs.

```purescript
do v <- makeVar
   forkAff (later $ putVar v 1.0)
   a <- takeVar v 
   liftEff $ trace ("Succeeded with " ++ show a)
```

You can use these constructs as one-sided blocking queues, which suspend (if 
necessary) on `take` operations, or as asynchronous, empty-or-full variables.

## Parallel Execution

If you only need the power of `Applicative`, then instead of using the monadic `Aff`, you can use the `Par` newtype wrapper defined in `Control.Monad.Aff.Par`.

This provides parallel instances of `Apply` and `Alt`.

In the following example, two Ajax requests are initiated simultaneously (rather than in sequence, as they would be for `Aff`):

```purescript
runPar (f <$> Par (Ajax.get "http://foo.com") <*> Par (Ajax.get "http://foo.com"))
```

The `(<|>)` operator of the `Alt` instance of `Par` allows you to race two asynchronous computations, and use whichever value comes back first (or the first error, if both err).

The `runPar` function allows you to unwrap the `Aff` and return to normal monadic (sequential) composition.

A parallel computation can be canceled if both of its individual components can be canceled.

# API Docs

[MODULES.md](MODULES.md)
