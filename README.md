# purescript-aff

An asynchronous effect monad for PureScript.

The moral equivalent of `ErrorT (ContT Unit (Eff e)) a`, for effects `e` and value `a`. Just faster, easier to use, and self-contained.

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

## Escaping Callback Hell

Hopefully, you're using libraries that already use the `Aff` type, so you don't even have to think about callbacks!

If you're building your own library, or you have to interact with some native code that expects callbacks, then *purescript-aff* provides a `makeAff` function you can use:

```purescript
makeAff :: forall e a. ((Error -> Eff e Unit) -> (a -> Eff e Unit) -> Eff e Unit) -> Aff e a
```

This function expects you to provide a handler, which should call a user-supplied error callback or success callback with the result of the asynchronous computation.

For example, let's say we have an AJAX request function:

```purescript
ajaxGet :: forall e. (Response -> Eff (ajax :: Ajax | e) Unit) -> Request -> Eff (ajax :: Ajax | e) Unit
```

We can wrap this into an asynchronous computation like so:

```purescript
ajaxGet' :: forall e. Request -> Aff (ajax :: Ajax | e) Response
ajaxGet' req = makeAff (\error success -> ajaxGet success req)
```

This eliminates "callback hell" and allows us to write code simply using `do` notation:

```
do response <- ajaxGet' req
   liftEff $ trace response.body
```

## Converting from Eff

All synchronous computations (`Eff`) can be converted to asynchronous computations (`Aff`) with `liftEff` defined in `Control.Monad.Eff.Class` (see [here](https://github.com/paf31/purescript-monad-eff)).

```purescript
import Control.Monad.Eff.Class

liftEff $ trace "Hello world!"
```

## Exceptions

The `Aff` monad has error handling baked-in, so ordinarily you don't have to worry about it.

If you want to "attempt" a computation but recover from failure, you can use the `attempt` function:

```purescript
attempt :: forall e a. Aff e a -> Aff e (Either Error a)
```

This returns an `Either Error a` you can use to recover gracefully from failure.

With synchronous computations, sometimes the effect of throwing exceptions is embedded in the type. For example:

```purescript
saveFile :: forall e. String -> Bytes -> Eff (err :: Exception | e) Unit
```

If such a signature "leaks" into an asynchronous computation, you can remove the exception from the effect row by using the `catch` function:

```purescript
let asyncComp' = catch asyncComp
```

Once you `catch` the exception, any synchronous exception thrown by the code will now be propagated through the error channel of the asynchronous computation, and can be dealt with normally using `attempt`.

# Documentation

[MODULES.md](MODULES.md)
