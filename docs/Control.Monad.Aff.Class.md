## Module Control.Monad.Aff.Class

#### `MonadAff`

``` purescript
class MonadAff e m where
  liftAff :: forall a. Aff e a -> m a
```

##### Instances
``` purescript
MonadAff e (Aff e)
(Monad m, MonadAff eff m) => MonadAff eff (ContT r m)
(Monad m, MonadAff eff m) => MonadAff eff (ExceptT e m)
(Monad m, MonadAff eff m) => MonadAff eff (ListT m)
(Monad m, MonadAff eff m) => MonadAff eff (MaybeT m)
(Monad m, MonadAff eff m) => MonadAff eff (ReaderT r m)
(Monad m, Monoid w, MonadAff eff m) => MonadAff eff (RWST r w s m)
(Monad m, MonadAff eff m) => MonadAff eff (StateT s m)
(Monad m, Monoid w, MonadAff eff m) => MonadAff eff (WriterT w m)
```


