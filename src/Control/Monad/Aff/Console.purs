module Control.Monad.Aff.Console where

import Prelude
import Control.Monad.Eff.Console as C

import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Class (liftEff)

-- | Logs any string to the console. This basically saves you
-- | from writing `liftEff $ log x` everywhere.
log :: forall e. String -> Aff (console :: C.CONSOLE | e) Unit
log = liftEff <<< C.log

-- | Logs any `Show`-able value to the console. This basically saves you
-- | from writing `liftEff $ logShow x` everywhere.
logShow :: forall e a. (Show a) => a -> Aff (console :: C.CONSOLE | e) Unit
logShow = liftEff <<< C.logShow
