module Control.Monad.Aff.Class where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Cont.Trans (ContT)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.List.Trans (ListT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.RWS.Trans (RWST)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT)

import Data.Monoid (class Monoid)

class MonadEff eff m <= MonadAff eff m | m -> eff where
  liftAff :: forall a. Aff eff a -> m a

instance monadAffAff :: MonadAff e (Aff e) where
  liftAff = id

instance monadAffContT :: MonadAff eff m => MonadAff eff (ContT r m) where
  liftAff = lift <<< liftAff

instance monadAffExceptT :: MonadAff eff m => MonadAff eff (ExceptT e m) where
  liftAff = lift <<< liftAff

instance monadAffListT :: MonadAff eff m => MonadAff eff (ListT m) where
  liftAff = lift <<< liftAff

instance monadAffMaybe :: MonadAff eff m => MonadAff eff (MaybeT m) where
  liftAff = lift <<< liftAff

instance monadAffReader :: MonadAff eff m => MonadAff eff (ReaderT r m) where
  liftAff = lift <<< liftAff

instance monadAffRWS :: (MonadAff eff m, Monoid w) => MonadAff eff (RWST r w s m) where
  liftAff = lift <<< liftAff

instance monadAffState :: MonadAff eff m => MonadAff eff (StateT s m) where
  liftAff = lift <<< liftAff

instance monadAffWriter :: (MonadAff eff m, Monoid w) => MonadAff eff (WriterT w m) where
  liftAff = lift <<< liftAff
