module Control.Monad.Aff.Console where
  import Prelude
  import qualified Control.Monad.Eff.Console as C

  import Control.Monad.Aff
  import Control.Monad.Eff.Class(liftEff)

  -- | Logs any string to the console. This basically saves you
  -- | from writing `liftEff $ log x` everywhere.
  log :: forall e. String -> Aff (console :: C.CONSOLE | e) String
  log s = do
    liftEff $ C.log s
    return s

  -- | Prints any `Show`-able value to the console. This basically saves you 
  -- | from writing `liftEff $ print x` everywhere.
  print :: forall e a. (Show a) => a -> Aff (console :: C.CONSOLE | e) a
  print a = do
    liftEff $ C.print a
    return a
