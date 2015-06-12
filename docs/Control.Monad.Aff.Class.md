## Module Control.Monad.Aff.Class

#### `MonadAff`

``` purescript
class MonadAff e m where
  liftAff :: forall a. Aff e a -> m a
```

##### Instances
``` purescript
instance monadAffAff :: MonadAff e (Aff e)
```


