module Control.Monad.Aff.AVar
  ( module Control.Monad.Eff.AVar
  , makeVar
  , makeEmptyVar
  , isEmptyVar
  , takeVar
  , tryTakeVar
  , putVar
  , tryPutVar
  , readVar
  , tryReadVar
  , killVar
  ) where

import Prelude
import Control.Monad.Aff (Aff, Canceler(..), makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVar, AVAR)
import Control.Monad.Eff.AVar as AVar
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Data.Maybe (Maybe)

toCanceler ∷ ∀ eff. Eff eff Unit → Canceler eff
toCanceler = Canceler <<< const <<< liftEff

-- | Creates a fresh AVar with an initial value.
makeVar ∷ ∀ eff a. a → Aff (avar ∷ AVAR | eff) (AVar a)
makeVar = liftEff <<< AVar.makeVar

-- | Creates a fresh AVar.
makeEmptyVar ∷ ∀ eff a. Aff (avar ∷ AVAR | eff) (AVar a)
makeEmptyVar = liftEff AVar.makeEmptyVar

-- | Synchronously checks whether an AVar currently has a value.
isEmptyVar ∷ ∀ eff a. AVar a → Aff (avar ∷ AVAR | eff) Boolean
isEmptyVar = liftEff <<< AVar.isEmptyVar

-- | Takes the AVar value, leaving it empty. If the AVar is already empty,
-- | the callback will be queued until the AVar is filled. Multiple takes will
-- | resolve in order as the AVar fills.
takeVar ∷ ∀ eff a. AVar a → Aff (avar ∷ AVAR | eff) a
takeVar avar = makeAff \k → do
  c ← AVar.takeVar avar k
  pure (toCanceler c)

-- | Attempts to synchronously take an AVar value, leaving it empty. If the
-- | AVar is empty, this will return `Nothing`.
tryTakeVar ∷ ∀ eff a. AVar a → Aff (avar ∷ AVAR | eff) (Maybe a)
tryTakeVar = liftEff <<< AVar.tryTakeVar

-- | Sets the value of the AVar. If the AVar is already filled, it will be
-- | queued until the value is emptied. Multiple puts will resolve in order as
-- | the AVar becomes available.
putVar ∷ ∀ eff a. AVar a → a → Aff (avar ∷ AVAR | eff) Unit
putVar avar value = makeAff \k → do
  c ← AVar.putVar avar value k
  pure (toCanceler c)

-- | Attempts to synchronously fill an AVar. If the AVar is already filled,
-- | this will do nothing. Returns true or false depending on if it succeeded.
tryPutVar ∷ ∀ eff a. AVar a → a → Aff (avar ∷ AVAR | eff) Boolean
tryPutVar avar = liftEff <<< AVar.tryPutVar avar

-- | Reads the AVar value. Unlike `takeVar`, this will not leave the AVar empty.
-- | If the AVar is empty, this will queue until it is filled. Multiple reads
-- | will resolve at the same time, as soon as possible.
readVar ∷ ∀ eff a. AVar a → Aff (avar ∷ AVAR | eff) a
readVar avar = makeAff \k → do
  c ← AVar.readVar avar k
  pure (toCanceler c)

-- | Attempts to synchronously read an AVar. If the AVar is empty, this will
-- | return `Nothing`.
tryReadVar ∷ ∀ eff a. AVar a → Aff (avar ∷ AVAR | eff) (Maybe a)
tryReadVar = liftEff <<< AVar.tryReadVar

-- | Kills the AVar with an exception. All pending and future actions will
-- | resolve immediately with the provided exception.
killVar ∷ ∀ eff a. AVar a → Error → Aff (avar ∷ AVAR | eff) Unit
killVar avar = liftEff <<< AVar.killVar avar
