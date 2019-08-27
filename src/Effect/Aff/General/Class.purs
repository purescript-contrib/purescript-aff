module Effect.Aff.General.Class where

import Prelude
import Control.Monad.Cont.Trans (ContT)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.List.Trans (ListT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.RWS.Trans (RWST)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT)
import Effect.Aff.General (Aff)
import Effect.Class (class MonadEffect)

class MonadEffect m ⇐ MonadAff e m | m → e where
  liftAff ∷ Aff e ~> m

instance monadAffAff ∷ MonadAff e (Aff e) where
  liftAff = identity

instance monadAffContT ∷ MonadAff e m ⇒ MonadAff e (ContT r m) where
  liftAff = lift <<< liftAff

instance monadAffExceptT ∷ MonadAff e m ⇒ MonadAff e (ExceptT e m) where
  liftAff = lift <<< liftAff

instance monadAffListT ∷ MonadAff e m ⇒ MonadAff e (ListT m) where
  liftAff = lift <<< liftAff

instance monadAffMaybe ∷ MonadAff e m ⇒ MonadAff e (MaybeT m) where
  liftAff = lift <<< liftAff

instance monadAffReader ∷ MonadAff e m ⇒ MonadAff e (ReaderT r m) where
  liftAff = lift <<< liftAff

instance monadAffRWS ∷ (MonadAff e m, Monoid w) ⇒ MonadAff e (RWST r w s m) where
  liftAff = lift <<< liftAff

instance monadAffState ∷ MonadAff e m ⇒ MonadAff e (StateT s m) where
  liftAff = lift <<< liftAff

instance monadAffWriter ∷ (MonadAff e m, Monoid w) ⇒ MonadAff e (WriterT w m) where
  liftAff = lift <<< liftAff
