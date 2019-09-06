module Effect.Aff.Class where

import Control.Monad.Cont (ContT, lift)
import Control.Monad.Except (ExceptT)
import Control.Monad.List.Trans (ListT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.RWS (RWST)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Effect.Aff.General (Aff, Error)
import Effect.Class (class MonadEffect)
import Prelude (class Monoid, type (~>), identity, (<<<))

class MonadEffect m ⇐ MonadAff m where
  liftAff ∷ Aff Error ~> m

instance monadAffAff ∷ MonadAff (Aff Error) where
  liftAff = identity

instance monadAffContT ∷ MonadAff m ⇒ MonadAff (ContT r m) where
  liftAff = lift <<< liftAff

instance monadAffExceptT ∷ MonadAff m ⇒ MonadAff (ExceptT Error m) where
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
