module Effect.Aff.General
  ( Aff
  , Fiber
  , FiberStatus(..)
  , ParAff(..)
  , Canceler(..)
  , AffResult(..)
  , isInterrupted
  , isFailed
  , isSucceeded
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
  , catch
  , finally
  , invincible
  , killFiber
  , joinFiber
  , tryJoinFiber
  , liftEffect'
  , unsafeLiftEffect
  , cancelWith
  , bracket
  , panic
  , BracketConditions
  , generalBracket
  , nonCanceler
  , effectCanceler
  , fiberCanceler
  , status
  , lmapFlipped
  , (#!)
  , absurdL
  , absurdR
  , wrapL
  , wrapL'
  , unwrapL
  , unwrapL'
  , wrapR
  , wrapR'
  , unwrapR
  , unwrapR'
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
import Data.Bifunctor (class Bifunctor, bimap, lmap)
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

instance bifunctorAff ∷ Bifunctor Aff where
  bimap f g m = catch (map g m) (throwError <<< f)

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

data AffResult e a
  = Succeeded a
  | Failed e
  | Interrupted Error

derive instance functorAffResult ∷ Functor (AffResult e)

instance showAffResult ∷ (Show a, Show e) ⇒ Show (AffResult e a) where
  show (Succeeded a) = "(Succeeded " <> show a <> ")"
  show (Failed e) = "(Failed " <> show e <> ")"
  show (Interrupted a) = "(Interrupted " <> show a <> ")"

instance bifunctorAffResult ∷ Bifunctor AffResult where
  bimap _ g (Succeeded a)  = Succeeded (g a)
  bimap f _ (Failed e)     = Failed (f e)
  bimap _ _ (Interrupted e) = Interrupted e

isInterrupted ∷ ∀ e a. AffResult e a → Boolean
isInterrupted = case _ of
  Interrupted _ → true
  _             → false

isFailed ∷ ∀ e a. AffResult e a → Boolean
isFailed = case _ of
  Failed _ → true
  _        → false

isSucceeded ∷ ∀ e a. AffResult e a → Boolean
isSucceeded = case _ of
  Succeeded _ → true
  _           → false

type OnComplete e a =
  { rethrow ∷ Boolean
  , handler ∷ (AffResult e a → Effect Unit) → Effect Unit
  }

-- | Represents a forked computation by way of `forkAff`. `Fiber`s are
-- | memoized, so their results are only computed once.
newtype Fiber e a = Fiber
  { run ∷ Effect Unit
  , kill ∷ ∀ e. Fn.Fn2 Error (AffResult e Unit → Effect Unit) (Effect (Effect Unit))
  , join ∷ (AffResult e a → Effect Unit) → Effect (Effect Unit)
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
killFiber ∷ ∀ e1 e2 a. Error → Fiber e1 a → Aff e2 Unit
killFiber e (Fiber t) = _liftEffect t.isSuspended >>= if _
  then _liftEffect $ void $ Fn.runFn2 t.kill e (const (pure unit))
  else makeAff \k → effectCanceler <$> Fn.runFn2 t.kill e k

-- | Blocks until the fiber completes, yielding the result. If the fiber
-- | throws an exception, it is rethrown in the current fiber.
joinFiber ∷ ∀ e. Fiber e ~> Aff e
joinFiber = tryJoinFiber >=> case _ of
  Interrupted e → panic e
  Failed e      → throwError e
  Succeeded a   → pure a

tryJoinFiber ∷ ∀ e1 e2 a. Fiber e1 a → Aff e2 (AffResult e1 a)
tryJoinFiber (Fiber t) = makeAff \k → effectCanceler <$> t.join (k <<< Succeeded)

-- | Allows safely throwing to the error channel.
liftEffect' ∷ ∀ e a. Effect (AffResult e a) → Aff e a
liftEffect' = _liftEffectResult

-- | Assumes that any thrown error is of type e.
unsafeLiftEffect ∷ ∀ e a. Effect a → Aff e a
unsafeLiftEffect = _liftEffectUnsafe

data FiberStatus e a
  = Suspended
  | Completed (AffResult e a)
  | Running
  | Dying Error

derive instance functorFiberStatus ∷ Functor (FiberStatus e)

instance bifunctorFiberStatus ∷ Bifunctor FiberStatus where
  bimap f g (Completed x) = Completed (bimap f g x)
  bimap _ _ Suspended = Suspended
  bimap _ _ Running = Running
  bimap _ _ (Dying e) = Dying e

instance showFiberStatus ∷ (Show e, Show a) ⇒ Show (FiberStatus e a) where
  show Suspended = "Suspended"
  show (Completed x) = "(Completed " <> show x <> ")"
  show Running = "Running"
  show (Dying e) = "(Dying " <> show e <> ")"

status ∷ ∀ e a. Fiber e a → Effect (FiberStatus e a)
status (Fiber t) = t.status

-- | A cancellation effect for actions run via `makeAff`. If a `Fiber` is
-- | killed, and an async action is pending, the canceler will be called to
-- | clean it up.
newtype Canceler = Canceler (Error → Aff Void Unit)

derive instance newtypeCanceler ∷ Newtype Canceler _

instance semigroupCanceler ∷ Semigroup Canceler where
  append (Canceler c1) (Canceler c2) =
    Canceler \err → parSequence_ [ c1 err, c2 err ]

-- | A no-op `Canceler` can be constructed with `mempty`.
instance monoidCanceler ∷ Monoid Canceler where
  mempty = nonCanceler

-- | A canceler which does not cancel anything.
nonCanceler ∷ Canceler
nonCanceler = Canceler (const (pure unit))

-- | A canceler from an Effect action.
effectCanceler ∷ Effect Unit → Canceler
effectCanceler = Canceler <<< const <<< liftEffect

-- | A canceler from a Fiber.
fiberCanceler ∷ ∀ e a. Fiber e a → Canceler
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
delay (Milliseconds n) = Fn.runFn2 _delay Succeeded n

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
finally ∷ ∀ e a. Aff Void Unit → Aff e a → Aff e a
finally fin a = bracket (pure unit) (const fin) (const a)

-- | Runs an effect such that it cannot be killed.
invincible ∷ ∀ e a. Aff e a → Aff e a
invincible a = bracket a (const (pure unit)) pure

-- | Attaches a custom `Canceler` to an action. If the computation is canceled,
-- | then the custom `Canceler` will be run afterwards.
cancelWith ∷ ∀ e a. Aff e a → Canceler → Aff e a
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
bracket ∷ ∀ e a b. Aff e a → (a → Aff Void Unit) → (a → Aff e b) → Aff e b
bracket acquire completed =
  generalBracket acquire
    { killed: const completed
    , failed: const completed
    , completed: const completed
    }

panic ∷ ∀ e a. Error → Aff e a
panic = _panic

absurdL ∷ ∀ f a b. Bifunctor f ⇒ f Void b → f a b
absurdL = unsafeCoerce -- lmap absurd

absurdR ∷ ∀ f a b. Bifunctor f ⇒ f a Void → f a b
absurdR = unsafeCoerce -- rmap absurd

wrapL ∷ ∀ f a b c. Bifunctor f ⇒ Newtype a b ⇒ f b c → f a c
wrapL = unsafeCoerce -- lmap wrap

wrapL' ∷ ∀ f a b c. Bifunctor f ⇒ Newtype a b ⇒ (b → a) → f b c → f a c
wrapL' _ = unsafeCoerce -- lmap wrap

unwrapL ∷ ∀ f a b c. Bifunctor f ⇒ Newtype a b ⇒ f a c → f b c
unwrapL = unsafeCoerce -- lmap unwrap

unwrapL' ∷ ∀ f a b c. Bifunctor f ⇒ Newtype a b ⇒ (b → a) → f a c → f b c
unwrapL' _ = unsafeCoerce -- lmap unwrap

wrapR ∷ ∀ f a b c. Bifunctor f ⇒ Newtype a b ⇒ f c b → f c a
wrapR = unsafeCoerce -- rmap wrap

unwrapR ∷ ∀ f a b c. Bifunctor f ⇒ Newtype a b ⇒ f c a → f c b
unwrapR = unsafeCoerce -- rmap unwrap

wrapR' ∷ ∀ f a b c. Bifunctor f ⇒ Newtype a b ⇒ (b → a) → f c b → f c a
wrapR' _ = unsafeCoerce -- rmap wrap

unwrapR' ∷ ∀ f a b c. Bifunctor f ⇒ Newtype a b ⇒ (b → a) → f c a → f c b
unwrapR' _ = unsafeCoerce -- rmap unwrap

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

  killAll ∷ ∀ e2. Error → Supervised e a → Aff e2 Unit
  killAll err sup = makeAff \k →
    Fn.runFn3 _killAll err sup.supervisor (k (Succeeded unit))

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
foreign import _delay ∷ ∀ e a. Fn.Fn2 (Unit → AffResult a Unit) Number (Aff e Unit)
foreign import _liftEffect ∷ ∀ e a. Effect a → Aff e a
foreign import _liftEffectResult ∷ ∀ e a. Effect (AffResult e a) → Aff e a
foreign import _liftEffectUnsafe ∷ ∀ e a. Effect a → Aff e a
foreign import _parAffMap ∷ ∀ e a b. (a → b) → ParAff e a → ParAff e b
foreign import _parAffApply ∷ ∀ e a b. ParAff e (a → b) → ParAff e a → ParAff e b
foreign import _parAffAlt ∷ ∀ e a. (e → e → e) → ParAff e a → ParAff e a → ParAff e a
foreign import _makeFiber ∷ ∀ e a. Fn.Fn2 FFIUtil (Aff e a) (Effect (Fiber e a))
foreign import _makeSupervisedFiber ∷ ∀ e a. Fn.Fn2 FFIUtil (Aff e a) (Effect (Supervised e a))
foreign import _killAll ∷ Fn.Fn3 Error Supervisor (Effect Unit) (Effect Canceler)
foreign import _sequential ∷ ∀ e. ParAff e ~> Aff e
foreign import _panic ∷ ∀ e a. Error → Aff e a

type BracketConditions e a b =
  { killed ∷ Error → a → Aff Void Unit
  , failed ∷ e → a → Aff Void Unit
  , completed ∷ b → a → Aff Void Unit
  }

-- | A general purpose bracket which lets you observe the status of the
-- | bracketed action. The bracketed action may have been killed with an
-- | exception, thrown an exception, or completed successfully.
foreign import generalBracket ∷ ∀ e a b. Aff e a → BracketConditions e a b → (a → Aff e b) → Aff e b

-- | Constructs an `Aff` from low-level `Effect` effects using a callback. A
-- | `Canceler` effect should be returned to cancel the pending action. The
-- | supplied callback may be invoked only once. Subsequent invocation are
-- | ignored.
foreign import makeAff ∷ ∀ e a. ((AffResult e a → Effect Unit) → Effect Canceler) → Aff e a

lmapFlipped ∷ ∀ f a1 b a2. Bifunctor f ⇒ f a1 b → (a1 → a2) → f a2 b
lmapFlipped = flip lmap

infixl 1 lmapFlipped as #!

makeFiber ∷ ∀ e a. Aff e a → Effect (Fiber e a)
makeFiber aff = Fn.runFn2 _makeFiber ffiUtil aff

newtype FFIUtil = FFIUtil
  { isInterrupted ∷ ∀ e a. AffResult e a → Boolean
  , isFailed ∷ ∀ e a. AffResult e a → Boolean
  , isSucceeded ∷ ∀ e a. AffResult e a → Boolean
  , fromInterrupted ∷ ∀ e b. AffResult e b → Error
  , fromFailed ∷ ∀ e a. AffResult e a → e
  , fromSucceeded ∷ ∀ e a. AffResult e a → a
  , succeeded ∷ ∀ e a. a → AffResult e a
  , failed ∷ ∀ e a. e → AffResult e a
  , interrupted ∷ ∀ e a. Error → AffResult e a
  , statusSuspended ∷ ∀ e a. FiberStatus e a
  , statusCompleted ∷ ∀ e a. AffResult e a → FiberStatus e a
  , statusRunning ∷ ∀ e a. FiberStatus e a
  , statusDying ∷ ∀ e a. Error → FiberStatus e a
  }

ffiUtil ∷ FFIUtil
ffiUtil = FFIUtil
  { isInterrupted
  , isFailed
  , isSucceeded
  , fromInterrupted: unsafeFromInterrupted
  , fromFailed: unsafeFromFailed
  , fromSucceeded: unsafeFromSucceeded
  , succeeded: Succeeded
  , failed: Failed
  , interrupted: Interrupted
  , statusSuspended: Suspended
  , statusCompleted: Completed
  , statusRunning: Running
  , statusDying: Dying
  }
  where
  unsafeFromInterrupted ∷ ∀ e a. AffResult e a → Error
  unsafeFromInterrupted = case _ of
    Interrupted e  → e
    Failed _       → unsafeCrashWith "unsafeFromInterrupted: Failed"
    Succeeded _    → unsafeCrashWith "unsafeFromInterrupted: Succeeded"

  unsafeFromFailed ∷ ∀ e a. AffResult e a → e
  unsafeFromFailed = case _ of
    Failed e       → e
    Interrupted _  → unsafeCrashWith "unsafeFromFailed: Interrupted"
    Succeeded _    → unsafeCrashWith "unsafeFromFailed: Succeeded"
  
  unsafeFromSucceeded ∷ ∀ e a. AffResult e a → a
  unsafeFromSucceeded = case _ of
    Succeeded a    → a
    Failed _       → unsafeCrashWith "unsafeFromSucceeded: Failed"
    Interrupted _  → unsafeCrashWith "unsafeFromSucceeded: Interrupted"
