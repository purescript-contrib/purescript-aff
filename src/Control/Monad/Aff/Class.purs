module Control.Monad.Aff.Class where

  import Prelude

  import Control.Monad.Aff
  import Control.Monad.Cont.Trans (ContT())
  import Control.Monad.Error.Trans (ErrorT())
  import Control.Monad.Except.Trans (ExceptT())
  import Control.Monad.List.Trans (ListT())
  import Control.Monad.Maybe.Trans (MaybeT())
  import Control.Monad.Reader.Trans (ReaderT())
  import Control.Monad.RWS.Trans (RWST())
  import Control.Monad.State.Trans (StateT())
  import Control.Monad.Trans (lift)
  import Control.Monad.Writer.Trans (WriterT())

  import Data.Monoid (Monoid)

  class MonadAff e m where
    liftAff :: forall a. Aff e a -> m a

  instance monadAffAff :: MonadAff e (Aff e) where
    liftAff = id

  instance monadAffContT :: (Monad m, MonadAff eff m) => MonadAff eff (ContT r m) where
    liftAff = lift <<< liftAff

  instance monadAffError :: (Monad m, MonadAff eff m) => MonadAff eff (ErrorT e m) where
    liftAff = lift <<< liftAff

  instance monadAffExceptT :: (Monad m, MonadAff eff m) => MonadAff eff (ExceptT e m) where
    liftAff = lift <<< liftAff

  instance monadAffListT :: (Monad m, MonadAff eff m) => MonadAff eff (ListT m) where
    liftAff = lift <<< liftAff

  instance monadAffMaybe :: (Monad m, MonadAff eff m) => MonadAff eff (MaybeT m) where
    liftAff = lift <<< liftAff

  instance monadAffReader :: (Monad m, MonadAff eff m) => MonadAff eff (ReaderT r m) where
    liftAff = lift <<< liftAff

  instance monadAffRWS :: (Monad m, Monoid w, MonadAff eff m) => MonadAff eff (RWST r w s m) where
    liftAff = lift <<< liftAff

  instance monadAffState :: (Monad m, MonadAff eff m) => MonadAff eff (StateT s m) where
    liftAff = lift <<< liftAff

  instance monadAffWriter :: (Monad m, Monoid w, MonadAff eff m) => MonadAff eff (WriterT w m) where
    liftAff = lift <<< liftAff
