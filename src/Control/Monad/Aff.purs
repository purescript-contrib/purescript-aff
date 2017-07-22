module Control.Monad.Aff
  ( Aff
  , Thread
  , ParAff(..)
  , Canceler(..)
  , nonCanceler
  , makeAff
  , launchAff
  , runAff
  , forkAff
  , liftEff'
  , bracket
  , delay
  , finally
  , atomically
  , killThread
  , joinThread
  ) where

import Prelude
import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (Error, EXCEPTION, error)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)
import Control.Monad.Eff.Ref.Unsafe (unsafeRunRef)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, catchError, try)
import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Parallel (parSequence_)
import Control.Parallel.Class (class Parallel)
import Control.Plus (class Plus, empty)
import Data.Either (Either(..), isLeft)
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds(..))
import Partial.Unsafe (unsafeCrashWith)

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

newtype ParAff eff a = ParAff (Aff eff a)

derive instance newtypeParAff ∷ Newtype (ParAff eff a) _

derive newtype instance functorParAff ∷ Functor (ParAff eff)

instance applyParAff ∷ Apply (ParAff eff) where
  apply (ParAff ff) (ParAff fa) = ParAff $ makeAff \k → do
    ref1 ← unsafeRunRef $ newRef Nothing
    ref2 ← unsafeRunRef $ newRef Nothing

    t1 ← launchAff do
      f ← try ff
      liftEff do
        ma ← unsafeRunRef $ readRef ref2
        case ma of
          Nothing → unsafeRunRef $ writeRef ref1 (Just f)
          Just a  → k (f <*> a)

    t2 ← launchAff do
      a ← try fa
      liftEff do
        mf ← unsafeRunRef $ readRef ref1
        case mf of
          Nothing → unsafeRunRef $ writeRef ref2 (Just a)
          Just f  → k (f <*> a)

    pure $ Canceler \err →
      parSequence_
        [ killThread err t1
        , killThread err t2
        ]

instance applicativeParAff ∷ Applicative (ParAff eff) where
  pure = ParAff <<< pure

instance semigroupParAff ∷ Semigroup a ⇒ Semigroup (ParAff eff a) where
  append = lift2 append

instance monoidParAff ∷ Monoid a ⇒ Monoid (ParAff eff a) where
  mempty = pure mempty

data AltStatus a
  = Pending
  | Completed a

instance altParAff ∷ Alt (ParAff eff) where
  alt = runAlt
    where
    runAlt ∷ ∀ a. ParAff eff a → ParAff eff a → ParAff eff a
    runAlt (ParAff a1) (ParAff a2) = ParAff $ makeAff \k → do
      ref ← unsafeRunRef $ newRef Nothing
      t1 ← launchAff a1
      t2 ← launchAff a2

      let
        completed ∷ Thread eff a → Either Error a → Aff eff Unit
        completed t res = do
          val ← liftEff $ unsafeRunRef $ readRef ref
          case val, res of
            _, Right _ → do
              killThread (error "Alt ParAff: early exit") t
              liftEff (k res)
            Nothing, _ →
              liftEff $ unsafeRunRef $ writeRef ref (Just res)
            Just res', _ →
              liftEff (k res')

      t3 ← launchAff $ completed t2 =<< try (joinThread t1)
      t4 ← launchAff $ completed t1 =<< try (joinThread t2)

      pure $ Canceler \err →
        parSequence_
          [ killThread err t3
          , killThread err t4
          , killThread err t1
          , killThread err t2
          ]

instance plusParAff ∷ Plus (ParAff e) where
  empty = ParAff empty

instance alternativeParAff ∷ Alternative (ParAff e)

instance parallelAff ∷ Parallel (ParAff eff) (Aff eff) where
  parallel = ParAff
  sequential (ParAff aff) = aff

newtype Thread eff a = Thread
  { kill ∷ Error → Aff eff Unit
  , join ∷ Aff eff a
  }

instance functorThread ∷ Functor (Thread eff) where
  map f (Thread { kill, join }) = Thread { kill, join: f <$> join }

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

runAff ∷ ∀ eff a. (Either Error a → Eff eff Unit) → Aff eff a → Eff eff Unit
runAff k aff = void $ launchAff $ liftEff <<< k =<< try aff

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
foreign import bracket ∷ ∀ eff a b. Aff eff a → (a → Aff eff Unit) → (a → Aff eff b) → Aff eff b
foreign import makeAff ∷ ∀ eff a. ((Either Error a → Eff eff Unit) → Eff eff (Canceler eff)) → Aff eff a

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

unsafeFromLeft ∷ ∀ x y. Either x y → x
unsafeFromLeft = case _ of
  Left a  → a
  Right _ → unsafeCrashWith "unsafeFromLeft: Right"

unsafeFromRight ∷ ∀ x y. Either x y → y
unsafeFromRight = case _ of
  Right a → a
  Left  _ → unsafeCrashWith "unsafeFromRight: Left"
