## Module Control.Monad.Aff.Class

#### `MonadAff`

``` purescript
class MonadAff e m where
  liftAff :: forall a. Aff e a -> m a
```

##### Instances
``` purescript
instance monadAffAff :: MonadAff e (Aff e)
instance monadAffContT :: (Monad m, MonadAff eff m) => MonadAff eff (ContT r m)
instance monadAffError :: (Monad m, MonadAff eff m) => MonadAff eff (ErrorT e m)
instance monadAffExceptT :: (Monad m, MonadAff eff m) => MonadAff eff (ExceptT e m)
instance monadAffListT :: (Monad m, MonadAff eff m) => MonadAff eff (ListT m)
instance monadAffMaybe :: (Monad m, MonadAff eff m) => MonadAff eff (MaybeT m)
instance monadAffReader :: (Monad m, MonadAff eff m) => MonadAff eff (ReaderT r m)
instance monadAffRWS :: (Monad m, Monoid w, MonadAff eff m) => MonadAff eff (RWST r w s m)
instance monadAffState :: (Monad m, MonadAff eff m) => MonadAff eff (StateT s m)
instance monadAffWriter :: (Monad m, Monoid w, MonadAff eff m) => MonadAff eff (WriterT w m)
```


