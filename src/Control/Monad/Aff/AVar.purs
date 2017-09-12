module Control.Monad.Aff.AVar
  ( module Control.Monad.Eff.AVar
  , makeVar
  , makeEmptyVar
  , status
  , isEmptyVar
  , isFilledVar
  , isKilledVar
  , takeVar
  , tryTakeVar
  , putVar
  , tryPutVar
  , readVar
  , tryReadVar
  , killVar
  ) where

import Prelude
import Control.Monad.Aff (Aff, makeAff, effCanceler)
import Control.Monad.Eff.AVar (AVar, AVAR, AVarStatus(..), isEmpty, isFilled, isKilled)
import Control.Monad.Eff.AVar as AVar
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Data.Maybe (Maybe)

-- | Creates a fresh AVar with an initial value.
makeVar ∷ ∀ eff a. a → Aff (avar ∷ AVAR | eff) (AVar a)
makeVar = liftEff <<< AVar.makeVar

-- | Creates a fresh AVar.
makeEmptyVar ∷ ∀ eff a. Aff (avar ∷ AVAR | eff) (AVar a)
makeEmptyVar = liftEff AVar.makeEmptyVar

-- | Synchronously checks the status of an AVar.
status ∷ ∀ eff a. AVar a → Aff (avar ∷ AVAR | eff) (AVar.AVarStatus a)
status = liftEff <<< AVar.status

-- | Synchronously checks whether an AVar currently is empty.
isEmptyVar ∷ ∀ eff a. AVar a → Aff (avar ∷ AVAR | eff) Boolean
isEmptyVar = liftEff <<< AVar.isEmptyVar

-- | Synchronously checks whether an AVar currently has a value.
isFilledVar ∷ ∀ eff a. AVar a → Aff (avar ∷ AVAR | eff) Boolean
isFilledVar = liftEff <<< AVar.isFilledVar

-- | Synchronously checks whether an AVar has been killed.
isKilledVar ∷ ∀ eff a. AVar a → Aff (avar ∷ AVAR | eff) Boolean
isKilledVar = liftEff <<< AVar.isKilledVar

-- | Takes the AVar value, leaving it empty. If the AVar is already empty,
-- | the callback will be queued until the AVar is filled. Multiple takes will
-- | resolve in order as the AVar fills.
takeVar ∷ ∀ eff a. AVar a → Aff (avar ∷ AVAR | eff) a
takeVar avar = makeAff \k → do
  c ← AVar.takeVar avar k
  pure (effCanceler c)

-- | Attempts to synchronously take an AVar value, leaving it empty. If the
-- | AVar is empty, this will return `Nothing`.
tryTakeVar ∷ ∀ eff a. AVar a → Aff (avar ∷ AVAR | eff) (Maybe a)
tryTakeVar = liftEff <<< AVar.tryTakeVar

-- | Sets the value of the AVar. If the AVar is already filled, it will be
-- | queued until the value is emptied. Multiple puts will resolve in order as
-- | the AVar becomes available.
putVar ∷ ∀ eff a. a → AVar a → Aff (avar ∷ AVAR | eff) Unit
putVar value avar = makeAff \k → do
  c ← AVar.putVar value avar k
  pure (effCanceler c)

-- | Attempts to synchronously fill an AVar. If the AVar is already filled,
-- | this will do nothing. Returns true or false depending on if it succeeded.
tryPutVar ∷ ∀ eff a. a → AVar a → Aff (avar ∷ AVAR | eff) Boolean
tryPutVar value = liftEff <<< AVar.tryPutVar value

-- | Reads the AVar value. Unlike `takeVar`, this will not leave the AVar empty.
-- | If the AVar is empty, this will queue until it is filled. Multiple reads
-- | will resolve at the same time, as soon as possible.
readVar ∷ ∀ eff a. AVar a → Aff (avar ∷ AVAR | eff) a
readVar avar = makeAff \k → do
  c ← AVar.readVar avar k
  pure (effCanceler c)

-- | Attempts to synchronously read an AVar. If the AVar is empty, this will
-- | return `Nothing`.
tryReadVar ∷ ∀ eff a. AVar a → Aff (avar ∷ AVAR | eff) (Maybe a)
tryReadVar = liftEff <<< AVar.tryReadVar

-- | Kills the AVar with an exception. All pending and future actions will
-- | resolve immediately with the provided exception.
killVar ∷ ∀ eff a. Error → AVar a → Aff (avar ∷ AVAR | eff) Unit
killVar error = liftEff <<< AVar.killVar error
