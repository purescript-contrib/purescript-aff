module Test.Main where

import Prelude
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Data.Either (Either(..))
import Test.Assert (assert', ASSERT)

type TestEffects eff = (assert ∷ ASSERT, console ∷ CONSOLE | eff)
type TestEff eff = Eff (TestEffects eff)
type TestAff eff = Aff (TestEffects eff)

runAssertEq ∷ ∀ eff a. Show a ⇒ Eq a ⇒ String → a → TestAff eff a → TestEff eff Unit
runAssertEq s a = runAff go
  where
  go (Left err) = do
    Console.error ("[Error] " <> s)
    assert' s false
  go (Right r) = do
    assert' s (r == a)
    Console.log ("[OK] " <> s)

test_pure ∷ ∀ eff. TestEff eff Unit
test_pure = runAssertEq "pure" 42 (pure 42)

main ∷ TestEff () Unit
main = do
  test_pure
