module Control.Monad.Aff.Class where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Cont.Trans (ContT)
import Control.Monad.Effect.Class (class MonadEffect)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.List.Trans (ListT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.RWS.Trans (RWST)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT)
import Data.Monoid (class Monoid)

class MonadEffect m ⇐ MonadAff m where
  liftAff ∷ Aff ~> m

instance monadAffAff ∷ MonadAff Aff where
  liftAff = id

instance monadAffContT ∷ MonadAff m ⇒ MonadAff (ContT r m) where
  liftAff = lift <<< liftAff

instance monadAffExceptT ∷ MonadAff m ⇒ MonadAff (ExceptT e m) where
  liftAff = lift <<< liftAff

instance monadAffListT ∷ MonadAff m ⇒ MonadAff (ListT m) where
  liftAff = lift <<< liftAff

instance monadAffMaybe ∷ MonadAff m ⇒ MonadAff (MaybeT m) where
  liftAff = lift <<< liftAff

instance monadAffReader ∷ MonadAff m ⇒ MonadAff (ReaderT r m) where
  liftAff = lift <<< liftAff

instance monadAffRWS ∷ (MonadAff m, Monoid w) ⇒ MonadAff (RWST r w s m) where
  liftAff = lift <<< liftAff

instance monadAffState ∷ MonadAff m ⇒ MonadAff (StateT s m) where
  liftAff = lift <<< liftAff

instance monadAffWriter ∷ (MonadAff m, Monoid w) ⇒ MonadAff (WriterT w m) where
  liftAff = lift <<< liftAff
