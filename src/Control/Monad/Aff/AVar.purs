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

makeVar ∷ ∀ eff a. a → Aff (avar ∷ AVAR | eff) (AVar a)
makeVar = liftEff <<< AVar.makeVar

makeEmptyVar ∷ ∀ eff a. Aff (avar ∷ AVAR | eff) (AVar a)
makeEmptyVar = liftEff AVar.makeEmptyVar

isEmptyVar ∷ ∀ eff a. AVar a → Aff (avar ∷ AVAR | eff) Boolean
isEmptyVar = liftEff <<< AVar.isEmptyVar

takeVar ∷ ∀ eff a. AVar a → Aff (avar ∷ AVAR | eff) a
takeVar avar = makeAff \k → do
  c ← AVar.takeVar avar k
  pure (toCanceler c)

tryTakeVar ∷ ∀ eff a. AVar a → Aff (avar ∷ AVAR | eff) (Maybe a)
tryTakeVar = liftEff <<< AVar.tryTakeVar

putVar ∷ ∀ eff a. AVar a → a → Aff (avar ∷ AVAR | eff) Unit
putVar avar value = makeAff \k → do
  c ← AVar.putVar avar value k
  pure (toCanceler c)

tryPutVar ∷ ∀ eff a. AVar a → a → Aff (avar ∷ AVAR | eff) Boolean
tryPutVar avar = liftEff <<< AVar.tryPutVar avar

readVar ∷ ∀ eff a. AVar a → Aff (avar ∷ AVAR | eff) a
readVar avar = makeAff \k → do
  c ← AVar.readVar avar k
  pure (toCanceler c)

tryReadVar ∷ ∀ eff a. AVar a → Aff (avar ∷ AVAR | eff) (Maybe a)
tryReadVar = liftEff <<< AVar.tryReadVar

killVar ∷ ∀ eff a. AVar a → Error → Aff (avar ∷ AVAR | eff) Unit
killVar avar = liftEff <<< AVar.killVar avar
