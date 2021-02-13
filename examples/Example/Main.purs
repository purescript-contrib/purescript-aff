module Example.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Example.Parallel.Syntax as P

main :: Effect Unit
main = do
  log "Running Parallel.Syntax example"
  P.main
