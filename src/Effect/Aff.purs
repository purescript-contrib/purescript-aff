module Effect.Aff
  ( Aff
  , Canceler
  , BracketConditions
  , Fiber
  , ParAff
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
  , generalBracket
  , nonCanceler
  , effectCanceler
  , fiberCanceler
  , module Exports
  )
where

import Control.Monad.Error.Class (throwError)
import Control.Monad.Error.Class (try, throwError, catchError) as Exports
import Control.Parallel.Class (sequential, parallel) as Exports
import Data.Either (Either, either)
import Data.Time.Duration (Milliseconds(..)) as Exports
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Aff.General as G
import Effect.Exception (Error)
import Effect.Exception (Error, error, message) as Exports
import Prelude (type (~>), Unit, map, pure, (<<<), (>=>))

type Aff = G.Aff Error

type Canceler = G.Canceler

type BracketConditions a b = G.BracketConditions Error a b

type Fiber = G.Fiber Error

type ParAff = G.ParAff Error

generalBracket ∷ ∀ a b. Aff a → BracketConditions a b → (a → Aff b) → Aff b
generalBracket = G.generalBracket

makeAff ∷ ∀ a. ((Either Error a → Effect Unit) → Effect Canceler) → Aff a
makeAff f = G.makeAff (\g → f (g <<< either G.Failed G.Succeeded))

-- | Invokes pending cancelers in a fiber and runs cleanup effects. Blocks
-- | until the fiber has fully exited.
killFiber ∷ ∀ a. Error → Fiber a → Aff Unit
killFiber = G.killFiber

-- | Blocks until the fiber completes, yielding the result. If the fiber
-- | throws an exception, it is rethrown in the current fiber.
joinFiber ∷ Fiber ~> Aff
joinFiber = G.tryJoinFiber >=> case _ of
  G.Interrupted e → throwError e
  G.Failed e      → throwError e
  G.Succeeded a   → pure a

-- | Allows safely throwing to the error channel.
liftEffect' ∷ ∀ a. Effect (Either Error a) → Aff a
liftEffect' = G.liftEffect' <<< map (either G.Failed G.Succeeded)

-- | Assumes that any thrown error is of type e.
unsafeLiftEffect ∷ ∀ a. Effect a → Aff a
unsafeLiftEffect = G.unsafeLiftEffect

-- | A canceler which does not cancel anything.
nonCanceler ∷ Canceler
nonCanceler = G.nonCanceler

-- | A canceler from an Effect action.
effectCanceler ∷ Effect Unit → Canceler
effectCanceler = G.effectCanceler

-- | A canceler from a Fiber.
fiberCanceler ∷ ∀ a. Fiber a → Canceler
fiberCanceler = G.fiberCanceler

-- | Forks an `Aff` from an `Effect` context, returning the `Fiber`.
launchAff ∷ ∀ a. Aff a → Effect (Fiber a)
launchAff = G.launchAff

-- | Forks an `Aff` from an `Effect` context, discarding the `Fiber`.
launchAff_ ∷ ∀ a. Aff a → Effect Unit
launchAff_ = G.launchAff_

-- | Suspends an `Aff` from an `Effect` context, returning the `Fiber`.
launchSuspendedAff ∷ ∀ a. Aff a → Effect (Fiber a)
launchSuspendedAff = G.launchSuspendedAff

-- | Forks an `Aff` from an `Effect` context and also takes a callback to run when
-- | it completes. Returns the pending `Fiber`.
runAff ∷ ∀ a. (Either Error a → Effect Unit) → Aff a → Effect (Fiber Unit)
runAff = G.runAff

-- | Forks an `Aff` from an `Effect` context and also takes a callback to run when
-- | it completes, discarding the `Fiber`.
runAff_ ∷ ∀ a. (Either Error a → Effect Unit) → Aff a → Effect Unit
runAff_ = G.runAff_

-- | Suspends an `Aff` from an `Effect` context and also takes a callback to run
-- | when it completes. Returns the suspended `Fiber`.
runSuspendedAff ∷ ∀ a. (Either Error a → Effect Unit) → Aff a → Effect (Fiber Unit)
runSuspendedAff = G.runSuspendedAff

-- | Forks am `Aff` from within a parent `Aff` context, returning the `Fiber`.
forkAff ∷ ∀ a. Aff a → Aff (Fiber a)
forkAff = G.forkAff

-- | Suspends an `Aff` from within a parent `Aff` context, returning the `Fiber`.
-- | A suspended `Aff` is not executed until a consumer observes the result
-- | with `joinFiber`.
suspendAff ∷ ∀ a. Aff a → Aff (Fiber a)
suspendAff = G.suspendAff

-- | Pauses the running fiber.
delay ∷ Milliseconds → Aff Unit
delay = G.delay

-- | An async computation which does not resolve.
never ∷ ∀ a. Aff a
never = G.never

-- | A monomorphic version of `try` that can map the error type. Catches thrown
-- | errors and lifts them into an `Either`.
attempt ∷ ∀ a. Aff a → Aff (Either Error a)
attempt = G.attempt

-- | Ignores any errors.
apathize ∷ ∀ a. Aff a → Aff Unit
apathize = G.apathize

-- | Runs the first effect after the second, regardless of whether it completed
-- | successfully or the fiber was cancelled.
finally ∷ ∀ a. Aff Unit → Aff a → Aff a
finally = G.finally <<< G.apathize

-- | Runs an effect such that it cannot be killed.
invincible ∷ ∀ a. Aff a → Aff a
invincible = G.invincible

-- | Attaches a custom `Canceler` to an action. If the computation is canceled,
-- | then the custom `Canceler` will be run afterwards.
cancelWith ∷ ∀ a. Aff a → Canceler → Aff a
cancelWith = G.cancelWith

-- | Guarantees resource acquisition and cleanup. The first effect may acquire
-- | some resource, while the second will dispose of it. The third effect makes
-- | use of the resource. Disposal is always run last, regardless. Neither
-- | acquisition nor disposal may be cancelled and are guaranteed to run until
-- | they complete.
bracket ∷ ∀ a b. Aff a → (a → Aff Unit) → (a → Aff b) → Aff b
bracket acquire release = G.bracket acquire (\a → G.catch (release a) G.panic)

-- | Creates a new supervision context for some `Aff`, guaranteeing fiber
-- | cleanup when the parent completes. Any pending fibers forked within
-- | the context will be killed and have their cancelers run.
supervise ∷ ∀ a. Aff a → Aff a
supervise = G.supervise
