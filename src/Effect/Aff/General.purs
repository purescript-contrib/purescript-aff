module Effect.Aff.General
  ( Aff
  , Fiber
  , FiberStatus(..)
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
  , supervise
  , attempt
  , apathize
  , delay
  , never
  , finally
  , invincible
  , killFiber
  , joinFiber
  , liftEffect'
  , unsafeLiftEffect
  , cancelWith
  , bracket
  , BracketConditions
  , generalBracket
  , nonCanceler
  , effectCanceler
  , fiberCanceler
  , status
  , module Exports
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Lazy (class Lazy)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, catchError, try)
import Control.Monad.Error.Class (try, throwError, catchError) as Exports
import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Control.Parallel (parSequence_, parallel)
import Control.Parallel.Class (class Parallel)
import Control.Parallel.Class (sequential, parallel) as Exports
import Control.Plus (class Plus, empty)
import Data.Either (Either(..))
import Data.Function.Uncurried as Fn
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds(..))
import Data.Time.Duration (Milliseconds(..)) as Exports
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error)
import Effect.Exception (Error, error, message) as Exports
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)

-- | An `Aff a` is an asynchronous computation with effects. The
-- | computation may either error with an exception, or produce a result of
-- | type `a`. `Aff` effects are assembled from primitive `Effect` effects using
-- | `makeAff` or `liftEffect`.
foreign import data Aff ∷ Type → Type → Type

instance functorAff ∷ Functor (Aff e) where
  map = _map

instance applyAff ∷ Apply (Aff e) where
  apply = ap

instance applicativeAff ∷ Applicative (Aff e) where
  pure = _pure

instance bindAff ∷ Bind (Aff e) where
  bind = _bind

instance monadAff ∷ Monad (Aff e)

instance semigroupAff ∷ Semigroup a ⇒ Semigroup (Aff e a) where
  append = lift2 append

instance monoidAff ∷ Monoid a ⇒ Monoid (Aff e a) where
  mempty = pure mempty

instance altAff ∷ Alt (Aff e) where
  alt a1 a2 = catchError a1 (const a2)

instance plusAff ∷ Monoid e ⇒ Plus (Aff e) where
  empty = throwError mempty

-- | This instance is provided for compatibility. `Aff` is always stack-safe
-- | within a given fiber. This instance will just result in unnecessary
-- | bind overhead.
instance monadRecAff ∷ MonadRec (Aff e) where
  tailRecM k = go
    where
    go a = do
      res ← k a
      case res of
        Done r → pure r
        Loop b → go b

instance monadThrowAff ∷ MonadThrow e (Aff e) where
  throwError = _throwError

instance monadErrorAff ∷ MonadError e (Aff e) where
  catchError = _catchError

instance monadEffectAff ∷ MonadEffect (Aff e) where
  liftEffect = _liftEffect

instance lazyAff ∷ Lazy (Aff e a) where
  defer f = pure unit >>= f

-- | Applicative for running parallel effects. Any `Aff` can be coerced to a
-- | `ParAff` and back using the `Parallel` class.
foreign import data ParAff ∷ Type → Type → Type

instance functorParAff ∷ Functor (ParAff e) where
  map = _parAffMap

-- | Runs effects in parallel, combining their results.
instance applyParAff ∷ Apply (ParAff e) where
  apply = _parAffApply

instance applicativeParAff ∷ Applicative (ParAff e) where
  pure = parallel <<< pure

instance semigroupParAff ∷ Semigroup a ⇒ Semigroup (ParAff e a) where
  append = lift2 append

instance monoidParAff ∷ Monoid a ⇒ Monoid (ParAff e a) where
  mempty = pure mempty

-- | Races effects in parallel. Losing branches will be cancelled.
instance altParAff ∷ Semigroup e ⇒ Alt (ParAff e) where
  alt = _parAffAlt append

instance plusParAff ∷ Monoid e ⇒ Plus (ParAff e) where
  empty = parallel empty

instance alternativeParAff ∷ Monoid e ⇒ Alternative (ParAff e)

instance parallelAff ∷ Parallel (ParAff e) (Aff e) where
  parallel = (unsafeCoerce ∷ ∀ a. Aff e a → ParAff e a)
  sequential = _sequential

type OnComplete e a =
  { rethrow ∷ Boolean
  , handler ∷ (Either e a → Effect Unit) → Effect Unit
  }

-- | Represents a forked computation by way of `forkAff`. `Fiber`s are
-- | memoized, so their results are only computed once.
newtype Fiber e a = Fiber
  { run ∷ Effect Unit
  , kill ∷ Fn.Fn2 Error (Either e Unit → Effect Unit) (Effect (Effect Unit))
  , join ∷ (Either e a → Effect Unit) → Effect (Effect Unit)
  , onComplete ∷ OnComplete e a → Effect (Effect Unit)
  , isSuspended ∷ Effect Boolean
  , status ∷ Effect (FiberStatus e a)
  }

instance functorFiber ∷ Functor (Fiber e) where
  map f t = unsafePerformEffect (makeFiber (f <$> joinFiber t))

instance applyFiber ∷ Apply (Fiber e) where
  apply t1 t2 = unsafePerformEffect (makeFiber (joinFiber t1 <*> joinFiber t2))

instance applicativeFiber ∷ Applicative (Fiber e) where
  pure a = unsafePerformEffect (makeFiber (pure a))

-- | Invokes pending cancelers in a fiber and runs cleanup effects. Blocks
-- | until the fiber has fully exited.
killFiber ∷ ∀ e a. Error → Fiber e a → Aff e Unit
killFiber e (Fiber t) = _liftEffect t.isSuspended >>= if _
  then _liftEffect $ void $ Fn.runFn2 t.kill e (const (pure unit))
  else makeAff \k → effectCanceler <$> Fn.runFn2 t.kill e k

-- | Blocks until the fiber completes, yielding the result. If the fiber
-- | throws an exception, it is rethrown in the current fiber.
joinFiber ∷ ∀ e. Fiber e ~> Aff e
joinFiber (Fiber t) = makeAff \k → effectCanceler <$> t.join k

-- | Allows safely throwing to the error channel.
liftEffect' ∷ ∀ e a. Effect (Either e a) → Aff e a
liftEffect' = _liftEffectEither

-- | Assumes that any thrown error is of type e.
unsafeLiftEffect ∷ ∀ e a. Effect a → Aff e a
unsafeLiftEffect = _liftEffectUnsafe

data FiberStatus e a
  = Suspended
  | Completed (Either e a)
  | Running
  | Killed Error
  | Dying Error

status ∷ ∀ e a. Fiber e a → Effect (FiberStatus e a)
status (Fiber t) = t.status

-- | A cancellation effect for actions run via `makeAff`. If a `Fiber` is
-- | killed, and an async action is pending, the canceler will be called to
-- | clean it up.
newtype Canceler e = Canceler (Error → Aff e Unit)

derive instance newtypeCanceler ∷ Newtype (Canceler e) _

instance semigroupCanceler ∷ Semigroup (Canceler e) where
  append (Canceler c1) (Canceler c2) =
    Canceler \err → parSequence_ [ c1 err, c2 err ]

-- | A no-op `Canceler` can be constructed with `mempty`.
instance monoidCanceler ∷ Monoid (Canceler e) where
  mempty = nonCanceler

-- | A canceler which does not cancel anything.
nonCanceler ∷ ∀ e. Canceler e
nonCanceler = Canceler (const (pure unit))

-- | A canceler from an Effect action.
effectCanceler ∷ ∀ e. Effect Unit → Canceler e
effectCanceler = Canceler <<< const <<< liftEffect

-- | A canceler from a Fiber.
fiberCanceler ∷ ∀ e a. Fiber e a → Canceler e
fiberCanceler = Canceler <<< flip killFiber

-- | Forks an `Aff` from an `Effect` context, returning the `Fiber`.
launchAff ∷ ∀ e a. Aff e a → Effect (Fiber e a)
launchAff aff = do
  fiber ← makeFiber aff
  case fiber of Fiber f → f.run
  pure fiber

-- | Forks an `Aff` from an `Effect` context, discarding the `Fiber`.
launchAff_ ∷ ∀ e a. Aff e a → Effect Unit
launchAff_ = void <<< launchAff

-- | Suspends an `Aff` from an `Effect` context, returning the `Fiber`.
launchSuspendedAff ∷ ∀ e a. Aff e a → Effect (Fiber e a)
launchSuspendedAff = makeFiber

-- | Forks an `Aff` from an `Effect` context and also takes a callback to run when
-- | it completes. Returns the pending `Fiber`.
runAff ∷ ∀ e a. (Either e a → Effect Unit) → Aff e a → Effect (Fiber e Unit)
runAff k aff = launchAff $ liftEffect <<< k =<< try aff

-- | Forks an `Aff` from an `Effect` context and also takes a callback to run when
-- | it completes, discarding the `Fiber`.
runAff_ ∷ ∀ e a. (Either e a → Effect Unit) → Aff e a → Effect Unit
runAff_ k aff = void $ runAff k aff

-- | Suspends an `Aff` from an `Effect` context and also takes a callback to run
-- | when it completes. Returns the suspended `Fiber`.
runSuspendedAff ∷ ∀ e a. (Either e a → Effect Unit) → Aff e a → Effect (Fiber e Unit)
runSuspendedAff k aff = launchSuspendedAff $ liftEffect <<< k =<< try aff

-- | Forks am `Aff` from within a parent `Aff` context, returning the `Fiber`.
forkAff ∷ ∀ e1 e2 a. Aff e1 a → Aff e2 (Fiber e1 a)
forkAff = _fork true

-- | Suspends an `Aff` from within a parent `Aff` context, returning the `Fiber`.
-- | A suspended `Aff` is not executed until a consumer observes the result
-- | with `joinFiber`.
suspendAff ∷ ∀ e1 e2 a. Aff e1 a → Aff e2 (Fiber e1 a)
suspendAff = _fork false

-- | Pauses the running fiber.
delay ∷ ∀ e. Milliseconds → Aff e Unit
delay (Milliseconds n) = Fn.runFn2 _delay Right n

-- | An async computation which does not resolve.
never ∷ ∀ e a. Aff e a
never = makeAff \_ → pure mempty

-- | A version of `catchError` that can map the error type.
catch ∷ ∀ e1 e2 a. Aff e1 a → (e1 → Aff e2 a) → Aff e2 a
catch = _catchError

-- | A monomorphic version of `try` that can map the error type. Catches thrown
-- | errors and lifts them into an `Either`.
attempt ∷ ∀ e1 e2 a. Aff e1 a → Aff e2 (Either e1 a)
attempt m = catch (Right <$> m) (pure <<< Left)

-- | Ignores any errors.
apathize ∷ ∀ e e' a. Aff e a → Aff e' Unit
apathize = attempt >>> map (const unit)

-- | Runs the first effect after the second, regardless of whether it completed
-- | successfully or the fiber was cancelled.
finally ∷ ∀ e a. Aff e Unit → Aff e a → Aff e a
finally fin a = bracket (pure unit) (const fin) (const a)

-- | Runs an effect such that it cannot be killed.
invincible ∷ ∀ e a. Aff e a → Aff e a
invincible a = bracket a (const (pure unit)) pure

-- | Attaches a custom `Canceler` to an action. If the computation is canceled,
-- | then the custom `Canceler` will be run afterwards.
cancelWith ∷ ∀ e a. Aff e a → Canceler e → Aff e a
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
bracket ∷ ∀ e a b. Aff e a → (a → Aff e Unit) → (a → Aff e b) → Aff e b
bracket acquire completed =
  generalBracket acquire
    { killed: const completed
    , failed: const completed
    , completed: const completed
    }

type Supervised e a =
  { fiber ∷ Fiber e a
  , supervisor ∷ Supervisor
  }

-- | Creates a new supervision context for some `Aff`, guaranteeing fiber
-- | cleanup when the parent completes. Any pending fibers forked within
-- | the context will be killed and have their cancelers run.
supervise ∷ ∀ e a. Aff e a → Aff e a
supervise aff =
  generalBracket (_liftEffect acquire)
    { killed: \err sup → parSequence_ [ killFiber err sup.fiber, killAll err sup ]
    , failed: const (killAll killError)
    , completed: const (killAll killError)
    }
    (joinFiber <<< _.fiber)
  where
  killError ∷ Error
  killError =
    error "[Aff] Child fiber outlived parent"

  killAll ∷ Error → Supervised e a → Aff e Unit
  killAll err sup = makeAff \k →
    Fn.runFn3 _killAll err sup.supervisor (k (pure unit))

  acquire ∷ Effect (Supervised e a)
  acquire = do
    sup ← Fn.runFn2 _makeSupervisedFiber ffiUtil aff
    case sup.fiber of Fiber f → f.run
    pure sup

foreign import data Supervisor ∷ Type
foreign import _pure ∷ ∀ e a. a → Aff e a
foreign import _throwError ∷ ∀ e a. e → Aff e a
foreign import _catchError ∷ ∀ e1 e2 a. Aff e1 a → (e1 → Aff e2 a) → Aff e2 a
foreign import _fork ∷ ∀ e1 e2 a. Boolean → Aff e1 a → Aff e2 (Fiber e1 a)
foreign import _map ∷ ∀ e a b. (a → b) → Aff e a → Aff e b
foreign import _bind ∷ ∀ e a b. Aff e a → (a → Aff e b) → Aff e b
foreign import _delay ∷ ∀ e a. Fn.Fn2 (Unit → Either a Unit) Number (Aff e Unit)
foreign import _liftEffect ∷ ∀ e a. Effect a → Aff e a
foreign import _liftEffectEither ∷ ∀ e a. Effect (Either e a) → Aff e a
foreign import _liftEffectUnsafe ∷ ∀ e a. Effect a → Aff e a
foreign import _parAffMap ∷ ∀ e a b. (a → b) → ParAff e a → ParAff e b
foreign import _parAffApply ∷ ∀ e a b. ParAff e (a → b) → ParAff e a → ParAff e b
foreign import _parAffAlt ∷ ∀ e a. (e → e → e) → ParAff e a → ParAff e a → ParAff e a
foreign import _makeFiber ∷ ∀ e a. Fn.Fn2 FFIUtil (Aff e a) (Effect (Fiber e a))
foreign import _makeSupervisedFiber ∷ ∀ e a. Fn.Fn2 FFIUtil (Aff e a) (Effect (Supervised e a))
foreign import _killAll ∷ ∀ e. Fn.Fn3 Error Supervisor (Effect Unit) (Effect (Canceler e))
foreign import _sequential ∷ ∀ e. ParAff e ~> Aff e

type BracketConditions e a b =
  { killed ∷ Error → a → Aff e Unit
  , failed ∷ e → a → Aff e Unit
  , completed ∷ b → a → Aff e Unit
  }

-- | A general purpose bracket which lets you observe the status of the
-- | bracketed action. The bracketed action may have been killed with an
-- | exception, thrown an exception, or completed successfully.
foreign import generalBracket ∷ ∀ e a b. Aff e a → BracketConditions e a b → (a → Aff e b) → Aff e b

-- | Constructs an `Aff` from low-level `Effect` effects using a callback. A
-- | `Canceler` effect should be returned to cancel the pending action. The
-- | supplied callback may be invoked only once. Subsequent invocation are
-- | ignored.
foreign import makeAff ∷ ∀ e a. ((Either e a → Effect Unit) → Effect (Canceler e)) → Aff e a

makeFiber ∷ ∀ e a. Aff e a → Effect (Fiber e a)
makeFiber aff = Fn.runFn2 _makeFiber ffiUtil aff

newtype FFIUtil = FFIUtil
  { isLeft ∷ ∀ a b. Either a b → Boolean
  , fromLeft ∷ ∀ a b. Either a b → a
  , fromRight ∷ ∀ a b. Either a b → b
  , left ∷ ∀ a b. a → Either a b
  , right ∷ ∀ a b. b → Either a b
  , statusSuspended ∷ ∀ e a. FiberStatus e a
  , statusCompleted ∷ ∀ e a. Either e a → FiberStatus e a
  , statusRunning ∷ ∀ e a. FiberStatus e a
  , statusKilled ∷ ∀ e a. Error → FiberStatus e a
  , statusDying ∷ ∀ e a. Error → FiberStatus e a
  }

ffiUtil ∷ FFIUtil
ffiUtil = FFIUtil
  { isLeft
  , fromLeft: unsafeFromLeft
  , fromRight: unsafeFromRight
  , left: Left
  , right: Right
  , statusSuspended: Suspended
  , statusCompleted: Completed
  , statusRunning: Running
  , statusKilled: Killed
  , statusDying: Dying
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
