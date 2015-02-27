# purescript-aff

An asynchronous effect monad for PureScript.

# Example

```purescript
main = launchAff $ 
  do response <- Ajax.get "http://foo.bar"
     liftEff $ trace response.body
```

# Documentation

[MODULES.md](MODULES.md)
