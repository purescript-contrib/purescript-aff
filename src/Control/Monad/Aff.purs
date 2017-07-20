module Control.Monad.Aff
  ( module Internal
  , module Control.Monad.Error.Class
  , liftEff'
  , forkAff
  , runAff
  ) where

import Prelude
import Control.Monad.Aff.Internal (ASYNC, Aff, Thread, launchAff, unsafeLaunchAff)
import Control.Monad.Aff.Internal (Aff, ParAff, Thread, Canceler(..), ASYNC, bracket, delay, launchAff, makeAff, nonCanceler, joinThread, killThread) as Internal
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Error.Class (try)
import Data.Either (Either)

liftEff' ∷ ∀ eff a. Eff (exception ∷ EXCEPTION | eff) a → Aff eff a
liftEff' = liftEff <<< unsafeCoerceEff

runAff ∷ ∀ eff a. (Either Error a → Eff eff Unit) → Aff eff a → Eff (async ∷ ASYNC | eff) Unit
runAff k aff = void $ launchAff $ liftEff <<< k =<< try aff

forkAff ∷ ∀ eff a. Aff eff a → Aff eff (Thread eff a)
forkAff = liftEff <<< unsafeLaunchAff
