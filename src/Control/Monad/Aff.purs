module Control.Monad.Aff
  ( Aff
  , Thread
  , ParAff(..)
  , Canceler(..)
  , nonCanceler
  , makeAff
  , launchAff
  , runAff
  , runAff_
  , forkAff
  , liftEff'
  , bracket
  , delay
  , finally
  , atomically
  , killThread
  , joinThread
  , module Exports
  ) where

import Prelude
import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (Error, EXCEPTION, error)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, catchError, try)
import Control.Monad.Error.Class (try) as Exports
import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Parallel (parSequence_, parallel)
import Control.Parallel.Class (class Parallel)
import Control.Plus (class Plus, empty)
import Data.Either (Either(..), isLeft)
import Data.Function.Uncurried as Fn
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds(..))
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)

-- | An `Aff eff a` is an asynchronous computation with effects `eff`. The
-- | computation may either error with an exception, or produce a result of
-- | type `a`. `Aff` effects are assembled from primitive `Eff` effects using
-- | `makeAff`.
foreign import data Aff ∷ # Effect → Type → Type

instance functorAff ∷ Functor (Aff eff) where
  map = _map

instance applyAff ∷ Apply (Aff eff) where
  apply = ap

instance applicativeAff ∷ Applicative (Aff eff) where
  pure = _pure

instance bindAff ∷ Bind (Aff eff) where
  bind = _bind

instance monadAff ∷ Monad (Aff eff)

instance semigroupAff ∷ Semigroup a ⇒ Semigroup (Aff eff a) where
  append = lift2 append

instance monoidAff ∷ Monoid a ⇒ Monoid (Aff eff a) where
  mempty = pure mempty

instance altAff ∷ Alt (Aff eff) where
  alt a1 a2 = catchError a1 (const a2)

instance plusAff ∷ Plus (Aff eff) where
  empty = throwError (error "Always fails")

instance alternativeAff ∷ Alternative (Aff eff)

instance monadZeroAff ∷ MonadZero (Aff eff)

instance monadPlusAff ∷ MonadPlus (Aff eff)

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
  catchError = _catchError

instance monadEffAff ∷ MonadEff eff (Aff eff) where
  liftEff = _liftEff

foreign import data ParAff ∷ # Effect → Type → Type

instance functorParAff ∷ Functor (ParAff eff) where
  map = _parAffMap

instance applyParAff ∷ Apply (ParAff eff) where
  apply = _parAffApply

instance applicativeParAff ∷ Applicative (ParAff eff) where
  pure = parallel <<< pure

instance semigroupParAff ∷ Semigroup a ⇒ Semigroup (ParAff eff a) where
  append = lift2 append

instance monoidParAff ∷ Monoid a ⇒ Monoid (ParAff eff a) where
  mempty = pure mempty

instance altParAff ∷ Alt (ParAff eff) where
  alt = _parAffAlt

instance plusParAff ∷ Plus (ParAff e) where
  empty = parallel empty

instance alternativeParAff ∷ Alternative (ParAff e)

instance parallelAff ∷ Parallel (ParAff eff) (Aff eff) where
  parallel = (unsafeCoerce ∷ ∀ a. Aff eff a → ParAff eff a)
  sequential a = Fn.runFn7 _sequential isLeft unsafeFromLeft unsafeFromRight Left Right runAff a

newtype Thread eff a = Thread
  { kill ∷ Error → Aff eff Unit
  , join ∷ Aff eff a
  }

instance functorThread ∷ Functor (Thread eff) where
  map f (Thread { kill, join }) = Thread
    { kill
    , join: memoAff (f <$> join)
    }

instance applyThread ∷ Apply (Thread eff) where
  apply t1 t2 = Thread
    { kill: \err → parSequence_ [ killThread err t1, killThread err t2 ]
    , join: memoAff (joinThread t1 <*> joinThread t2)
    }

instance applicativeThread ∷ Applicative (Thread eff) where
  pure a = Thread
    { kill: const (pure unit)
    , join: pure a
    }

killThread ∷ ∀ eff a. Error → Thread eff a → Aff eff Unit
killThread e (Thread t) = t.kill e

joinThread ∷ ∀ eff a. Thread eff a → Aff eff a
joinThread (Thread t) = t.join

newtype Canceler eff = Canceler (Error → Aff eff Unit)

derive instance newtypeCanceler ∷ Newtype (Canceler eff) _

instance semigroupCanceler ∷ Semigroup (Canceler eff) where
  append (Canceler c1) (Canceler c2) =
    Canceler \err → parSequence_ [ c1 err, c2 err ]

instance monoidCanceler ∷ Monoid (Canceler eff) where
  mempty = nonCanceler

nonCanceler ∷ ∀ eff. Canceler eff
nonCanceler = Canceler (const (pure unit))

launchAff ∷ ∀ eff a. Aff eff a → Eff eff (Thread eff a)
launchAff aff = Fn.runFn6 _launchAff isLeft unsafeFromLeft unsafeFromRight Left Right aff

runAff ∷ ∀ eff a. (Either Error a → Eff eff Unit) → Aff eff a → Eff eff (Thread eff Unit)
runAff k aff = launchAff $ liftEff <<< k =<< try aff

runAff_ ∷ ∀ eff a. (Either Error a → Eff eff Unit) → Aff eff a → Eff eff Unit
runAff_ k aff = void $ runAff k aff

forkAff ∷ ∀ eff a. Aff eff a → Aff eff (Thread eff a)
forkAff = liftEff <<< launchAff

delay ∷ ∀ eff. Milliseconds → Aff eff Unit
delay (Milliseconds n) = Fn.runFn2 _delay Right n

liftEff' ∷ ∀ eff a. Eff (exception ∷ EXCEPTION | eff) a → Aff eff a
liftEff' = liftEff <<< unsafeCoerceEff

finally ∷ ∀ eff a. Aff eff Unit → Aff eff a → Aff eff a
finally fin a = bracket (pure unit) (const fin) (const a)

atomically ∷ ∀ eff a. Aff eff a → Aff eff a
atomically a = bracket a (const (pure unit)) pure

foreign import _pure ∷ ∀ eff a. a → Aff eff a
foreign import _throwError ∷ ∀ eff a. Error → Aff eff a
foreign import _catchError ∷ ∀ eff a. Aff eff a → (Error → Aff eff a) → Aff eff a
foreign import _map ∷ ∀ eff a b. (a → b) → Aff eff a → Aff eff b
foreign import _bind ∷ ∀ eff a b. Aff eff a → (a → Aff eff b) → Aff eff b
foreign import _delay ∷ ∀ a eff. Fn.Fn2 (Unit → Either a Unit) Number (Aff eff Unit)
foreign import _liftEff ∷ ∀ eff a. Eff eff a → Aff eff a
foreign import _parAffMap ∷ ∀ eff a b. (a → b) → ParAff eff a → ParAff eff b
foreign import _parAffApply ∷ ∀ eff a b. ParAff eff (a → b) → ParAff eff a → ParAff eff b
foreign import _parAffAlt ∷ ∀ eff a. ParAff eff a → ParAff eff a → ParAff eff a
foreign import bracket ∷ ∀ eff a b. Aff eff a → (a → Aff eff Unit) → (a → Aff eff b) → Aff eff b
foreign import makeAff ∷ ∀ eff a. ((Either Error a → Eff eff Unit) → Eff eff (Canceler eff)) → Aff eff a
foreign import memoAff ∷ ∀ eff a. Aff eff a → Aff eff a

foreign import _launchAff
  ∷ ∀ eff a
  . Fn.Fn6
      (Either Error a → Boolean)
      (Either Error a → Error)
      (Either Error a → a)
      (Error → Either Error a)
      (a → Either Error a)
      (Aff eff a)
      (Eff eff (Thread eff a))

foreign import _sequential
  ∷ ∀ eff a
  . Fn.Fn7
      (Either Error a → Boolean)
      (Either Error a → Error)
      (Either Error a → a)
      (Error → Either Error a)
      (a → Either Error a)
      ((Either Error a → Eff eff Unit) → Aff eff a → Eff eff (Thread eff Unit))
      (ParAff eff a)
      (Aff eff a)

unsafeFromLeft ∷ ∀ x y. Either x y → x
unsafeFromLeft = case _ of
  Left a  → a
  Right _ → unsafeCrashWith "unsafeFromLeft: Right"

unsafeFromRight ∷ ∀ x y. Either x y → y
unsafeFromRight = case _ of
  Right a → a
  Left  _ → unsafeCrashWith "unsafeFromRight: Left"
