module Control.Monad.Aff
  ( module Internal
  , forkAff
  , runAff
  , killThread
  , joinThread
  ) where

import Prelude
import Control.Monad.Aff.Internal (ASYNC, Aff, AffModality, Thread(..), attempt, launchAff, unsafeLaunchAff, unsafeLiftEff)
import Control.Monad.Aff.Internal (Aff, AffModality, ParAff, Thread, Canceler(..), ASYNC, attempt, bracket, delay, launchAff, makeAff, nonCanceler) as Internal
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either)

runAff ∷ ∀ eff a. (Either Error a → Eff (AffModality eff) Unit) → Aff eff a → Eff (async ∷ ASYNC | eff) Unit
runAff k aff = void $ launchAff $ liftEff <<< k =<< attempt aff

forkAff ∷ ∀ eff a. Aff eff a → Aff eff (Thread eff a)
forkAff = unsafeLiftEff <<< unsafeLaunchAff

killThread ∷ ∀ eff a. Error → Thread eff a → Aff eff Unit
killThread e (Thread t) = t.kill e

joinThread ∷ ∀ eff a. Thread eff a → Aff eff a
joinThread (Thread t) = t.join
