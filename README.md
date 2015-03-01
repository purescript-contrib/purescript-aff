# purescript-aff

An asynchronous effect monad for PureScript.

The moral equivalent of `ErrorT (ContT Unit (Eff (async :: Async e1 | e2)) a`, for synchronous effects `e2`, asynchronous effects `e1`, and value `a`.

`Aff` lets you say goodbyte to monad transformers and callback hell!

# Example

```purescript
main = launchAff $ 
  do response <- Ajax.get "http://foo.bar"
     liftEff $ trace response.body
```

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

This looks like ordinary, synchronous, imperative code, but actually operates asynchronously without any callbacks (error handling is baked in so you only deal with it when you want to).

`Aff` represents a computation with a synchronous component (for example, initiating an AJAX request), as well as an asynchronous component (retrieving an error or success response).

For maximum type safety, `Aff` separates the effects of the synchronous part from the asynchronous part. That is, an `Aff` computation may have one set of effects for its synchronous component, and another set for its asynchronous part. 

Asynchronous effects are represented with an `async :: Async e1` effect label, where `e1` is the row of effects for actions to occur at some point in the future.

The library contains instances for `Semigroup`, `Monoid`, `Apply`, `Applicative`, `Bind`, `Monad`, `MonadEff`. and `MonadError`. These instances allow you to compose `Aff`-ectful code as easily as `Eff`, as well as interop with existing `Eff` code.

## Escaping Callback Hell

Hopefully, you're using libraries that already use the `Aff` type, so you don't even have to think about callbacks!

If you're building your own library, or you have to interact with some native code that expects callbacks, then *purescript-aff* provides a `makeAff` function you can use:

```purescript
makeAff :: forall e1 e2 a. ((Error -> Eff e1 Unit) -> (a -> Eff e1 Unit) -> EffA e1 e2 Unit) -> Aff e1 e2 a
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
""" :: forall e1 e2. (Response -> Eff e1 Unit) -> Request -> EffA e1 (ajax :: Ajax | e2) Unit
```

We can wrap this into an asynchronous computation like so:

```purescript
ajaxGet' :: forall e1 e2. Request -> Aff e1 (ajax :: Ajax | e2) Response
ajaxGet' req = makeAff (\error success -> ajaxGet success req)
```

This eliminates callback hell and allows us to write code simply using `do` notation:

```
do response <- ajaxGet' req
   liftEff $ trace response.body
```

## Converting from Eff

All purely synchronous computations (`Eff`) can be converted to `Aff` computations with `liftEff` defined in `Control.Monad.Eff.Class` (see [here](https://github.com/paf31/purescript-monad-eff)).

```purescript
import Control.Monad.Eff.Class

liftEff $ trace "Hello world!"
```

This lets you write your whole program in `Aff`, and still call out to synchronous `Eff` code.

## Exceptions

The `Aff` monad has error handling baked in, so ordinarily you don't have to worry about it.

If you want to attempt a computation but recover from failure, you can use the `attempt` function:

```purescript
attempt :: forall e1 e2 a. Aff e1 e2 a -> Aff e1 e2 (Either Error a)
```

This returns an `Either Error a` you can use to recover gracefully from failure.

`Aff` has a `MonadError` instance, which comes with two functions: `catchError`, and `throwError`.

These are defined in [purescript-transformers](http://github.com/purescript/purescript-transformers).
Here's an example of how you can use them:

```purescript
do resp <- (Ajax.get "http://foo.com") `catchError` (\e -> pure defaultResponse)
   if resp.statusCode != 200 then throwError myErr 
   else pure resp.body
```

Thrown exceptions are propagated on the error channel, and can be recovered from using `attempt` or `catchError`.

# Documentation

[MODULES.md](MODULES.md)
