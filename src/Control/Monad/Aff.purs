module Control.Monad.Aff
  ( Aff
  , Fiber
  , ParAff(..)
  , Canceler(..)
  , makeAff
  , launchAff
  , launchAff_
  , launchSuspendedAff
  , runAff
  , runAff_
  , runSuspendedAff
  , forkAff
  , suspendAff
  , liftEff'
  , supervise
  , attempt
  , apathize
  , delay
  , never
  , finally
  , invincible
  , killFiber
  , joinFiber
  , cancelWith
  , bracket
  , BracketConditions
  , generalBracket
  , nonCanceler
  , effCanceler
  , module Exports
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (Error, EXCEPTION, error)
import Control.Monad.Eff.Exception (Error, error, message) as Exports
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, catchError, try)
import Control.Monad.Error.Class (try, throwError, catchError) as Exports
import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Control.Parallel (parSequence_, parallel)
import Control.Parallel.Class (class Parallel)
import Control.Parallel.Class (sequential, parallel) as Exports
import Control.Plus (class Plus, empty)
import Data.Either (Either(..))
import Data.Function.Uncurried as Fn
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds(..))
import Data.Time.Duration (Milliseconds(..)) as Exports
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)

-- | An `Aff eff a` is an asynchronous computation with effects `eff`. The
-- | computation may either error with an exception, or produce a result of
-- | type `a`. `Aff` effects are assembled from primitive `Eff` effects using
-- | `makeAff` or `liftEff`.
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

-- | This instance is provided for compatibility. `Aff` is always stack-safe
-- | within a given fiber. This instance will just result in unnecessary
-- | bind overhead.
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

-- | Applicative for running parallel effects. Any `Aff` can be coerced to a
-- | `ParAff` and back using the `Parallel` class.
foreign import data ParAff ∷ # Effect → Type → Type

instance functorParAff ∷ Functor (ParAff eff) where
  map = _parAffMap

-- | Runs effects in parallel, combining their results.
instance applyParAff ∷ Apply (ParAff eff) where
  apply = _parAffApply

instance applicativeParAff ∷ Applicative (ParAff eff) where
  pure = parallel <<< pure

instance semigroupParAff ∷ Semigroup a ⇒ Semigroup (ParAff eff a) where
  append = lift2 append

instance monoidParAff ∷ Monoid a ⇒ Monoid (ParAff eff a) where
  mempty = pure mempty

-- | Races effects in parallel. Returns the first successful result or the
-- | first error if all fail with an exception. Losing branches will be
-- | cancelled.
instance altParAff ∷ Alt (ParAff eff) where
  alt = _parAffAlt

instance plusParAff ∷ Plus (ParAff e) where
  empty = parallel empty

instance alternativeParAff ∷ Alternative (ParAff e)

instance parallelAff ∷ Parallel (ParAff eff) (Aff eff) where
  parallel = (unsafeCoerce ∷ ∀ a. Aff eff a → ParAff eff a)
  sequential = _sequential

type OnComplete eff a =
  { rethrow ∷ Boolean
  , handler ∷ (Either Error a → Eff eff Unit) → Eff eff Unit
  }

-- | Represents a forked computation by way of `forkAff`. `Fiber`s are
-- | memoized, so their results are only computed once.
newtype Fiber eff a = Fiber
  { run ∷ Eff eff Unit
  , kill ∷ Fn.Fn2 Error (Either Error Unit → Eff eff Unit) (Eff eff (Eff eff Unit))
  , join ∷ (Either Error a → Eff eff Unit) → Eff eff (Eff eff Unit)
  , onComplete ∷ OnComplete eff a → Eff eff (Eff eff Unit)
  , isSuspended ∷ Eff eff Boolean
  }

instance functorFiber ∷ Functor (Fiber eff) where
  map f t = unsafePerformEff (makeFiber (f <$> joinFiber t))

instance applyFiber ∷ Apply (Fiber eff) where
  apply t1 t2 = unsafePerformEff (makeFiber (joinFiber t1 <*> joinFiber t2))

instance applicativeFiber ∷ Applicative (Fiber eff) where
  pure a = unsafePerformEff (makeFiber (pure a))

-- | Invokes pending cancelers in a fiber and runs cleanup effects. Blocks
-- | until the fiber has fully exited.
killFiber ∷ ∀ eff a. Error → Fiber eff a → Aff eff Unit
killFiber e (Fiber t) = liftEff t.isSuspended >>= if _
  then liftEff $ void $ Fn.runFn2 t.kill e (const (pure unit))
  else makeAff \k → effCanceler <$> Fn.runFn2 t.kill e k

-- | Blocks until the fiber completes, yielding the result. If the fiber
-- | throws an exception, it is rethrown in the current fiber.
joinFiber ∷ ∀ eff a. Fiber eff a → Aff eff a
joinFiber (Fiber t) = makeAff \k → effCanceler <$> t.join k

-- | A cancellation effect for actions run via `makeAff`. If a `Fiber` is
-- | killed, and an async action is pending, the canceler will be called to
-- | clean it up.
newtype Canceler eff = Canceler (Error → Aff eff Unit)

derive instance newtypeCanceler ∷ Newtype (Canceler eff) _

instance semigroupCanceler ∷ Semigroup (Canceler eff) where
  append (Canceler c1) (Canceler c2) =
    Canceler \err → parSequence_ [ c1 err, c2 err ]

-- | A no-op `Canceler` can be constructed with `mempty`.
instance monoidCanceler ∷ Monoid (Canceler eff) where
  mempty = nonCanceler

-- | A canceler which does not cancel anything.
nonCanceler ∷ ∀ eff. Canceler eff
nonCanceler = Canceler (const (pure unit))

-- | A canceler from an Eff action.
effCanceler ∷ ∀ eff. Eff eff Unit → Canceler eff
effCanceler = Canceler <<< const <<< liftEff

-- | Forks an `Aff` from an `Eff` context, returning the `Fiber`.
launchAff ∷ ∀ eff a. Aff eff a → Eff eff (Fiber eff a)
launchAff aff = do
  fiber ← makeFiber aff
  case fiber of Fiber f → f.run
  pure fiber

-- | Forks an `Aff` from an `Eff` context, discarding the `Fiber`.
launchAff_ ∷ ∀ eff a. Aff eff a → Eff eff Unit
launchAff_ = void <<< launchAff

-- | Suspends an `Aff` from an `Eff` context, returning the `Fiber`.
launchSuspendedAff ∷ ∀ eff a. Aff eff a → Eff eff (Fiber eff a)
launchSuspendedAff = makeFiber

-- | Forks an `Aff` from an `Eff` context and also takes a callback to run when
-- | it completes. Returns the pending `Fiber`.
runAff ∷ ∀ eff a. (Either Error a → Eff eff Unit) → Aff eff a → Eff eff (Fiber eff Unit)
runAff k aff = launchAff $ liftEff <<< k =<< try aff

-- | Forks an `Aff` from an `Eff` context and also takes a callback to run when
-- | it completes, discarding the `Fiber`.
runAff_ ∷ ∀ eff a. (Either Error a → Eff eff Unit) → Aff eff a → Eff eff Unit
runAff_ k aff = void $ runAff k aff

-- | Suspends an `Aff` from an `Eff` context and also takes a callback to run
-- | when it completes. Returns the suspended `Fiber`.
runSuspendedAff ∷ ∀ eff a. (Either Error a → Eff eff Unit) → Aff eff a → Eff eff (Fiber eff Unit)
runSuspendedAff k aff = launchSuspendedAff $ liftEff <<< k =<< try aff

-- | Forks am `Aff` from within a parent `Aff` context, returning the `Fiber`.
forkAff ∷ ∀ eff a. Aff eff a → Aff eff (Fiber eff a)
forkAff = _fork true

-- | Suspends an `Aff` from within a parent `Aff` context, returning the `Fiber`.
-- | A suspended `Aff` is not executed until a consumer observes the result
-- | with `joinFiber`.
suspendAff ∷ ∀ eff a. Aff eff a → Aff eff (Fiber eff a)
suspendAff = _fork false

-- | Pauses the running fiber.
delay ∷ ∀ eff. Milliseconds → Aff eff Unit
delay (Milliseconds n) = Fn.runFn2 _delay Right n

-- | An async computation which does not resolve.
never ∷ ∀ eff a. Aff eff a
never = makeAff \_ → pure mempty

-- | All `Eff` exceptions are implicitly caught within an `Aff` context, but
-- | standard `liftEff` won't remove the effect label.
liftEff' ∷ ∀ eff a. Eff (exception ∷ EXCEPTION | eff) a → Aff eff a
liftEff' = liftEff <<< unsafeCoerceEff

-- | A monomorphic version of `try`. Catches thrown errors and lifts them
-- | into an `Either`.
attempt ∷ ∀ eff a. Aff eff a → Aff eff (Either Error a)
attempt = try

-- | Ignores any errors.
apathize ∷ ∀ eff a. Aff eff a → Aff eff Unit
apathize = attempt >>> map (const unit)

-- | Runs the first effect after the second, regardless of whether it completed
-- | successfully or the fiber was cancelled.
finally ∷ ∀ eff a. Aff eff Unit → Aff eff a → Aff eff a
finally fin a = bracket (pure unit) (const fin) (const a)

-- | Runs an effect such that it cannot be killed.
invincible ∷ ∀ eff a. Aff eff a → Aff eff a
invincible a = bracket a (const (pure unit)) pure

-- | Attaches a custom `Canceler` to an action. If the computation is canceled,
-- | then the custom `Canceler` will be run afterwards.
cancelWith ∷ ∀ eff a. Aff eff a → Canceler eff → Aff eff a
cancelWith aff (Canceler cancel) =
  generalBracket (pure unit)
    { killed: \e _ → cancel e
    , failed: const pure
    , completed: const pure
    }
    (const aff)

-- | Guarantees resource acquisition and cleanup. The first effect may acquire
-- | some resource, while the second will dispose of it. The third effect makes
-- | use of the resource. Disposal is always run last, regardless. Neither
-- | acquisition nor disposal may be cancelled and are guaranteed to run until
-- | they complete.
bracket ∷ ∀ eff a b. Aff eff a → (a → Aff eff Unit) → (a → Aff eff b) → Aff eff b
bracket acquire completed =
  generalBracket acquire
    { killed: const completed
    , failed: const completed
    , completed: const completed
    }

type Supervised eff a =
  { fiber ∷ Fiber eff a
  , supervisor ∷ Supervisor eff
  }

-- | Creates a new supervision context for some `Aff`, guaranteeing fiber
-- | cleanup when the parent completes. Any pending fibers forked within
-- | the context will be killed and have their cancelers run.
supervise ∷ ∀ eff a. Aff eff a → Aff eff a
supervise aff =
  generalBracket (liftEff acquire)
    { killed: \err sup → parSequence_ [ killFiber err sup.fiber, killAll err sup ]
    , failed: const (killAll killError)
    , completed: const (killAll killError)
    }
    (joinFiber <<< _.fiber)
  where
  killError ∷ Error
  killError =
    error "[Aff] Child fiber outlived parent"

  killAll ∷ Error → Supervised eff a → Aff eff Unit
  killAll err sup = makeAff \k →
    Fn.runFn3 _killAll err sup.supervisor (k (pure unit))

  acquire ∷ Eff eff (Supervised eff a)
  acquire = do
    sup ← Fn.runFn2 _makeSupervisedFiber ffiUtil aff
    case sup.fiber of Fiber f → f.run
    pure sup

foreign import data Supervisor ∷ # Effect → Type
foreign import _pure ∷ ∀ eff a. a → Aff eff a
foreign import _throwError ∷ ∀ eff a. Error → Aff eff a
foreign import _catchError ∷ ∀ eff a. Aff eff a → (Error → Aff eff a) → Aff eff a
foreign import _fork ∷ ∀ eff a. Boolean → Aff eff a → Aff eff (Fiber eff a)
foreign import _map ∷ ∀ eff a b. (a → b) → Aff eff a → Aff eff b
foreign import _bind ∷ ∀ eff a b. Aff eff a → (a → Aff eff b) → Aff eff b
foreign import _delay ∷ ∀ a eff. Fn.Fn2 (Unit → Either a Unit) Number (Aff eff Unit)
foreign import _liftEff ∷ ∀ eff a. Eff eff a → Aff eff a
foreign import _parAffMap ∷ ∀ eff a b. (a → b) → ParAff eff a → ParAff eff b
foreign import _parAffApply ∷ ∀ eff a b. ParAff eff (a → b) → ParAff eff a → ParAff eff b
foreign import _parAffAlt ∷ ∀ eff a. ParAff eff a → ParAff eff a → ParAff eff a
foreign import _makeFiber ∷ ∀ eff a. Fn.Fn2 FFIUtil (Aff eff a) (Eff eff (Fiber eff a))
foreign import _makeSupervisedFiber ∷ ∀ eff a. Fn.Fn2 FFIUtil (Aff eff a) (Eff eff (Supervised eff a))
foreign import _killAll ∷ ∀ eff. Fn.Fn3 Error (Supervisor eff) (Eff eff Unit) (Eff eff (Canceler eff))
foreign import _sequential ∷ ∀ eff a. ParAff eff a → Aff eff a

type BracketConditions eff a b =
  { killed ∷ Error → a → Aff eff Unit
  , failed ∷ Error → a → Aff eff Unit
  , completed ∷ b → a → Aff eff Unit
  }

-- | A general purpose bracket which lets you observe the status of the
-- | bracketed action. The bracketed action may have been killed with an
-- | exception, thrown an exception, or completed successfully.
foreign import generalBracket ∷ ∀ eff a b. Aff eff a → BracketConditions eff a b → (a → Aff eff b) → Aff eff b

-- | Constructs an `Aff` from low-level `Eff` effects using a callback. A
-- | `Canceler` effect should be returned to cancel the pending action. The
-- | supplied callback may be invoked only once. Subsequent invocation are
-- | ignored.
foreign import makeAff ∷ ∀ eff a. ((Either Error a → Eff eff Unit) → Eff eff (Canceler eff)) → Aff eff a

makeFiber ∷ ∀ eff a. Aff eff a → Eff eff (Fiber eff a)
makeFiber aff = Fn.runFn2 _makeFiber ffiUtil aff

newtype FFIUtil = FFIUtil
  { isLeft ∷ ∀ a b. Either a b → Boolean
  , fromLeft ∷ ∀ a b. Either a b → a
  , fromRight ∷ ∀ a b. Either a b → b
  , left ∷ ∀ a b. a → Either a b
  , right ∷ ∀ a b. b → Either a b
  }

ffiUtil ∷ FFIUtil
ffiUtil = FFIUtil
  { isLeft
  , fromLeft: unsafeFromLeft
  , fromRight: unsafeFromRight
  , left: Left
  , right: Right
  }
  where
  isLeft ∷ ∀ a b. Either a b → Boolean
  isLeft = case _ of
    Left _ -> true
    Right _ → false

  unsafeFromLeft ∷ ∀ a b. Either a b → a
  unsafeFromLeft = case _ of
    Left a  → a
    Right _ → unsafeCrashWith "unsafeFromLeft: Right"

  unsafeFromRight ∷ ∀ a b. Either a b → b
  unsafeFromRight = case _ of
    Right a → a
    Left  _ → unsafeCrashWith "unsafeFromRight: Left"
