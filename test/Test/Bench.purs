module Test.Bench where

import Prelude
import Control.Monad.Aff as Aff
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Console as Console
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Performance.Minibench (bench)

loop1 ∷ ∀ eff. Int → Aff.Aff eff Int
loop1 = tailRecM go
  where
  go n
    | n <= 0    = pure $ Done n
    | otherwise = do
        _ ← do
          _ ← do
            _ ← pure n
            _ ← pure n
            _ ← pure n
            pure n
          _ ← pure n
          _ ← pure n
          _ ← pure n
          pure n
        pure $ Loop (n - 1)

loop2 ∷ ∀ eff. Int → Aff.Aff eff Int
loop2 = go
  where
  go n
    | n <= 0    = pure n
    | otherwise = do
        _ ← do
          _ ← do
            _ ← pure n
            _ ← pure n
            _ ← pure n
            pure n
          _ ← pure n
          _ ← pure n
          _ ← pure n
          pure n
        loop2 (n - 1)

fib1 ∷ ∀ e. Int → Aff.Aff e Int
fib1 n = if n <= 1 then pure n else do
  a ← fib1 (n - 1)
  b ← fib1 (n - 2)
  pure (a + b)

main ∷ Eff (console ∷ Console.CONSOLE) Unit
main = do
  Console.log "\nAff tailRecM:"
  bench \_ → runPure (void $ Aff.launchAff $ loop1 10000)

  Console.log "\nAff loop:"
  bench \_ → runPure (void $ Aff.launchAff $ loop2 10000)

  Console.log "\nAff fib:"
  bench \_ → runPure (void $ Aff.launchAff $ fib1 100)

