module Test.Bench where

import Prelude
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Performance.Minibench (bench)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Unsafe (unsafePerformEffect)
import Effect.Console as Console

loop1 ∷ Int → Aff.Aff Int
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

loop2 ∷ Int → Aff.Aff Int
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

fib1 ∷ Int → Aff.Aff Int
fib1 n = if n <= 1 then pure n else do
  a ← fib1 (n - 1)
  b ← fib1 (n - 2)
  pure (a + b)

main ∷ Effect Unit
main = do
  Console.log "\nAff tailRecM:"
  bench \_ → unsafePerformEffect $ void $ Aff.launchAff $ loop1 10000

  Console.log "\nAff loop:"
  bench \_ → unsafePerformEffect $ void $ Aff.launchAff $ loop2 10000

  Console.log "\nAff fib:"
  bench \_ → unsafePerformEffect $ void $ Aff.launchAff $ fib1 20

