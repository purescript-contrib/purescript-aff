# purescript-aff

An asynchronous effect monad for PureScript.

The moral equivalent of `ErrorT (ContT Unit (Eff e)) a`, for effects `e` and value `a`. Just faster, easier to use, and self-contained.

# Example

```purescript
main = launchAff $ 
  do response <- Ajax.get "http://foo.bar"
     liftEff $ trace response.body
```

# Documentation

[MODULES.md](MODULES.md)
