module Test.Main where

import Prelude

import Control.Monad.Aff (Aff(), runAff, later', forkAll)
import Control.Monad.Aff.AVar (AVAR(), makeVar', modifyVar, takeVar)
import Control.Monad.Cont.Class (callCC)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE(), log, print)
import Control.Monad.Eff.Exception (EXCEPTION(), throwException)
import Control.Monad.Rec.Class (tailRecM)

import Data.Array ((..))
import Data.Either (Either(..))

loop :: forall eff. Int -> Aff (console :: CONSOLE | eff) Unit
loop n = tailRecM go n
  where
  go 0 = do
    liftEff $ log "Done!"
    return (Right unit)
  go n = return (Left (n - 1))

all :: forall eff. Int -> Aff (console :: CONSOLE, avar :: AVAR | eff) Unit
all n = do
  var <- makeVar' 0
  forkAll $ map (\_ -> modifyVar (+ 1) var) (1 .. n)
  count <- takeVar var
  liftEff $ log ("Forked " <> show count)

delay :: forall eff. Int -> Aff eff Unit
delay n = callCC \cont ->
  later' n (cont unit)

main :: Eff (console :: CONSOLE, avar :: AVAR, err :: EXCEPTION) Unit
main = runAff throwException (const (pure unit)) $ do
  liftEff $ log "pre-delay"
  delay 1000
  liftEff $ log "post-delay"
  loop 1000000
  all 100000
