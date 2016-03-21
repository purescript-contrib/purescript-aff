module Control.Monad.Aff.Console where

import Prelude (class Show, Unit, (<<<))
import Control.Monad.Eff.Console as C

import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Class (liftEff)

-- | Logs any string to the console. This basically saves you
-- | from writing `liftEff $ log x` everywhere.
log :: forall e. String -> Aff (console :: C.CONSOLE | e) Unit
log = liftEff <<< C.log

-- | Prints any `Show`-able value to the console. This basically saves you
-- | from writing `liftEff $ print x` everywhere.
print :: forall e a. (Show a) => a -> Aff (console :: C.CONSOLE | e) Unit
print = liftEff <<< C.print
