module Control.Monad.Aff.Class where

import Prelude

import Control.Monad.Aff (Aff())
import Control.Monad.Cont.Trans (ContT())
import Control.Monad.Except.Trans (ExceptT())
import Control.Monad.Free (Free(), liftF)
import Control.Monad.List.Trans (ListT())
import Control.Monad.Maybe.Trans (MaybeT())
import Control.Monad.Reader.Trans (ReaderT())
import Control.Monad.RWS.Trans (RWST())
import Control.Monad.State.Trans (StateT())
import Control.Monad.Trans (lift)
import Control.Monad.Writer.Trans (WriterT())

import Data.Monoid (Monoid)

-- | A class for types that can carry an `Aff` value by some means.
class Affable e m where
  liftAff :: forall a. Aff e a -> m a

instance affableAff :: Affable e (Aff e) where
  liftAff = id

instance affableFree :: (Affable eff f) => Affable eff (Free f) where
  liftAff = liftF <<< liftAff

instance affableContT :: (Monad m, Affable eff m) => Affable eff (ContT r m) where
  liftAff = lift <<< liftAff

instance affableExceptT :: (Monad m, Affable eff m) => Affable eff (ExceptT e m) where
  liftAff = lift <<< liftAff

instance affableListT :: (Monad m, Affable eff m) => Affable eff (ListT m) where
  liftAff = lift <<< liftAff

instance affableMaybe :: (Monad m, Affable eff m) => Affable eff (MaybeT m) where
  liftAff = lift <<< liftAff

instance affableReader :: (Monad m, Affable eff m) => Affable eff (ReaderT r m) where
  liftAff = lift <<< liftAff

instance affableRWS :: (Monad m, Monoid w, Affable eff m) => Affable eff (RWST r w s m) where
  liftAff = lift <<< liftAff

instance affableState :: (Monad m, Affable eff m) => Affable eff (StateT s m) where
  liftAff = lift <<< liftAff

instance affableWriter :: (Monad m, Monoid w, Affable eff m) => Affable eff (WriterT w m) where
  liftAff = lift <<< liftAff

--| A class for types where the `Affable` instance for a `Monad` is also a monad
--| morphism.
class (Monad m, Affable e m) <= MonadAff e m

instance monadAffAff :: MonadAff e (Aff e)
instance monadAffContT :: (Monad m, Affable eff m) => MonadAff eff (ContT r m)
instance monadAffExceptT :: (Monad m, Affable eff m) => MonadAff eff (ExceptT e m)
instance monadAffListT :: (Monad m, Affable eff m) => MonadAff eff (ListT m)
instance monadAffMaybe :: (Monad m, Affable eff m) => MonadAff eff (MaybeT m)
instance monadAffReader :: (Monad m, Affable eff m) => MonadAff eff (ReaderT r m)
instance monadAffRWS :: (Monad m, Monoid w, Affable eff m) => MonadAff eff (RWST r w s m)
instance monadAffState :: (Monad m, Affable eff m) => MonadAff eff (StateT s m)
instance monadAffWriter :: (Monad m, Monoid w, Affable eff m) => MonadAff eff (WriterT w m)
