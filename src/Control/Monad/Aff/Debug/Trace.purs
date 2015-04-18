module Control.Monad.Aff.Debug.Trace where
  import qualified Debug.Trace as T

  import Control.Monad.Aff
  import Control.Monad.Eff.Class(liftEff)

  -- | Traces any `Show`-able value to the console. This basically saves you 
  -- | from writing `liftEff $ trace x` everywhere.
  trace :: forall e a. (Show a) => a -> Aff (trace :: T.Trace | e) a
  trace a = do
    liftEff $ T.trace (show a)
    return a
