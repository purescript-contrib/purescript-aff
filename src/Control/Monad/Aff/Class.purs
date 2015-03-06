module Control.Monad.Aff.Class where
  import Control.Monad.Aff

  class MonadAff e m where
    liftAff :: forall a. Aff e a -> m a

  instance monadAffAff :: MonadAff e (Aff e) where
    liftAff = id