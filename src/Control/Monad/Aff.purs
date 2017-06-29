module Control.Monad.Aff
  ( Aff
  , Thread
  , Canceler(..)
  , attempt
  , bracket
  , runAff
  , launchAff
  , forkAff
  , killThread
  , joinThread
  , onComplete
  ) where

import Prelude
import Data.Function.Uncurried as Fn
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Data.Either (Either(..), isLeft)
import Partial.Unsafe (unsafeCrashWith)
import Type.Row.Effect.Equality (class EffectRowEquals, effTo)

foreign import data Aff :: # Effect → Type → Type

instance functorAff ∷ Functor (Aff eff) where map = _map
instance applyAff ∷ Apply (Aff eff) where apply = ap
instance applicativeAff ∷ Applicative (Aff eff) where pure = _pure
instance bindAff ∷ Bind (Aff eff) where bind = _bind
instance monadAff ∷ Monad (Aff eff)

instance monadRecAff ∷ MonadRec (Aff eff) where
  tailRecM k = go
    where
    go a = do
      res ← k a
      case res of
        Done r → pure r
        Loop b → go b

instance monadThrowAff ∷ MonadThrow Error (Aff eff) where
  throwError = _throwError

instance monadErrorAff ∷ MonadError Error (Aff eff) where
  catchError aff k = do
    res ← attempt aff
    case res of
      Left err → k err
      Right r  → pure r

instance monadEffAff ∷ EffectRowEquals eff1 (exception ∷ EXCEPTION | eff2) ⇒ MonadEff eff1 (Aff eff2) where
  liftEff eff = Fn.runFn3 _liftEff Left Right (effTo eff)

newtype Thread eff a = Thread
  { kill ∷ Error → Aff eff Unit
  , join ∷ Aff eff a
  }

instance functorThread ∷ Functor (Thread eff) where
  map f (Thread { kill, join }) = Thread { kill, join: f <$> join }

newtype Canceler eff = Canceler (Error → Aff eff Unit)

attempt ∷ ∀ eff a. Aff eff a → Aff eff (Either Error a)
attempt = _attempt

bracket ∷ ∀ eff a b. Aff eff a → (a → Aff eff Unit) → (a → Aff eff b) → Aff eff b
bracket = _bracket

launchAff ∷ ∀ eff a. Aff eff a → Eff eff (Thread eff a)
launchAff aff = Fn.runFn6 _drainAff isLeft unsafeFromLeft unsafeFromRight Left Right aff
  where
  unsafeFromLeft ∷ ∀ x y. Either x y → x
  unsafeFromLeft = case _ of
    Left a → a
    Right  _ → unsafeCrashWith "unsafeFromLeft: Right"

  unsafeFromRight ∷ ∀ x y. Either x y → y
  unsafeFromRight = case _ of
    Right a → a
    Left  _ → unsafeCrashWith "unsafeFromRight: Left"

runAff ∷ ∀ eff a. (Either Error a → Eff (exception ∷ EXCEPTION | eff) Unit) → Aff eff a → Eff eff Unit
runAff k aff = do
  thread ← launchAff aff
  onComplete k thread

forkAff ∷ ∀ eff a. Aff eff a → Aff eff (Thread eff a)
forkAff = _unsafeSync <<< map Right <<< launchAff

killThread ∷ ∀ eff a. Error → Thread eff a → Aff eff Unit
killThread e (Thread t) = t.kill e

joinThread ∷ ∀ eff a. Thread eff a → Aff eff a
joinThread (Thread t) = t.join

onComplete ∷ ∀ eff a. (Either Error a → Eff (exception ∷ EXCEPTION | eff) Unit) → Thread eff a → Eff eff Unit
onComplete k t = void $ launchAff do
  res ← attempt (joinThread t)
  liftEff (k res)

foreign import _pure ∷ ∀ eff a. a → Aff eff a
foreign import _throwError ∷ ∀ eff a. Error → Aff eff a
foreign import _unsafeSync ∷ ∀ eff a. Eff eff (Either Error a) → Aff eff a
foreign import _unsafeAsync ∷ ∀ eff a. ((Either Error a → Eff eff Unit) → Eff eff (Canceler eff)) → Aff eff a
foreign import _map ∷ ∀ eff a b. (a → b) → Aff eff a → Aff eff b
foreign import _bind ∷ ∀ eff a b. Aff eff a → (a → Aff eff b) → Aff eff b
foreign import _attempt ∷ ∀ eff a. Aff eff a → Aff eff (Either Error a)
foreign import _bracket ∷ ∀ eff a b. Aff eff a → (a → Aff eff Unit) → (a → Aff eff b) → Aff eff b

foreign import _liftEff
  ∷ forall eff1 eff2 a
  . Fn.Fn3
      (Error → Either Error a)
      (a → Either Error a)
      (Eff (exception ∷ EXCEPTION | eff1) a)
      (Aff eff2 a)

foreign import _drainAff
  ∷ ∀ eff a
  . Fn.Fn6
      (Either Error a → Boolean)
      (Either Error a → Error)
      (Either Error a → a)
      (Error → Either Error a)
      (a → Either Error a)
      (Aff eff a)
      (Eff eff (Thread eff a))

