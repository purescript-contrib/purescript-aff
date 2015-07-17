module Test.Main where

import Prelude

import Control.Monad.Aff (Aff(), runAff, later')
import Control.Monad.Cont.Class (callCC)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE(), log, print)
import Control.Monad.Eff.Exception (EXCEPTION(), throwException)
import Control.Monad.Rec.Class (tailRecM)

import Data.Either (Either(..))

loop :: forall eff. Int -> Aff (console :: CONSOLE | eff) Unit
loop n = tailRecM go n
  where
  go 0 = do
    liftEff $ log "Done!"
    return (Right unit)
  go n = return (Left (n - 1))

delay :: forall eff. Int -> Aff eff Unit
delay n = callCC \cont ->
  later' n (cont unit)

main :: Eff (console :: CONSOLE, err :: EXCEPTION) Unit
main = runAff throwException (const (pure unit)) $ do
  liftEff $ log "pre-delay"
  delay 1000
  liftEff $ log "post-delay"
  loop 1000000
