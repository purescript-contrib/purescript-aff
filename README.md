# purescript-aff

An asynchronous effect monad for PureScript.

The moral equivalent of `ErrorT (ContT Unit (Eff (async :: Async e1 | e2)) a`, for synchronous effects `e1`, asynchronous effects `e2`, and value `a`.

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

## Escaping Callback Hell

Hopefully, you're using libraries that already use the `Aff` type, so you don't even have to think about callbacks!

If you're building your own library, or you have to interact with some native code that expects callbacks, then *purescript-aff* provides a `makeAff'` function you can use:

```purescript
makeAff' :: forall e1 e2 a. ((Error -> Eff e1 Unit) -> (a -> Eff e1 Unit) -> Eff e2 Unit) -> Aff e1 e2 a
```

This function expects you to provide a handler, which should call a user-supplied error callback or success callback with the result of the asynchronous computation.

For example, let's say we have an AJAX request function:

```purescript
ajaxGet :: forall e1 e2. (Response -> Eff e1 Unit) -> Request -> Eff (ajax :: Ajax | e2) Unit
```

We can wrap this into an asynchronous computation like so:

```purescript
ajaxGet' :: forall e1 e2. Request -> Aff e1 (ajax :: Ajax | e2) Response
ajaxGet' req = makeAff' (\error success -> ajaxGet success req)
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
attempt :: forall e1 e2 a. Aff e1 e2 a -> Aff e1 e2 (Either Error a)
```

This returns an `Either Error a` you can use to recover gracefully from failure.

The "opposite" of `attempt` is `throw`, which allows you to "throw" an exception:

```purescript
do resp <- Ajax.get "http://foo.com"
   if resp.statusCode != 200 then throw myErr 
   else pure resp.body
```

# Documentation

[MODULES.md](MODULES.md)
