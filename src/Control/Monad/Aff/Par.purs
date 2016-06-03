-- | A newtype over `Aff` that provides `Applicative` instances that run in
-- | parallel. This is useful, for example, if you want to run a whole bunch
-- | of AJAX requests at the same time, rather than sequentially.
module Control.Monad.Aff.Par
  ( Par(..)
  , runPar
  ) where

import Prelude
import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Aff (attempt, cancelWith, forkAff)
import Control.Monad.Aff.AVar (AffAVar, AVar, makeVar, makeVar', takeVar, putVar, killVar)
import Control.Monad.Eff.Exception (Error)
import Control.Plus (class Plus, empty)
import Data.Either (Either, either)
import Data.Monoid (class Monoid, mempty)

newtype Par e a = Par (AffAVar e a)

-- | Extracts the `Aff` from the `Par`.
runPar :: forall e a. Par e a -> AffAVar e a
runPar (Par aff) = aff

instance semigroupPar :: (Semigroup a) => Semigroup (Par e a) where
  append a b = append <$> a <*> b

instance monoidPar :: (Monoid a) => Monoid (Par e a) where
  mempty = pure mempty

instance functorPar :: Functor (Par e) where
  map f (Par fa) = Par (f <$> fa)

instance applyPar :: Apply (Par e) where
  apply (Par ff) (Par fa) = Par do
    let putOrKill :: forall b. AVar b -> Either Error b -> AffAVar e Unit
        putOrKill v = either (killVar v) (putVar v)
    vf <- makeVar
    va <- makeVar
    c1 <- forkAff (attempt ff >>= putOrKill vf)
    c2 <- forkAff (attempt fa >>= putOrKill va)
    (takeVar vf <*> takeVar va) `cancelWith` (c1 <> c2)

instance applicativePar :: Applicative (Par e) where
  pure v = Par (pure v)

-- | Returns the first value, or the first error if both error.
instance altPar :: Alt (Par e) where
  alt (Par a1) (Par a2) =
    let maybeKill va ve err = do
         e <- takeVar ve
         if e == 1 then killVar va err else pure unit
         putVar ve (e + 1)
    in Par do
      va <- makeVar     -- the `a` value
      ve <- makeVar' 0  -- the error count (starts at 0)
      c1 <- forkAff $ attempt a1 >>= either (maybeKill va ve) (putVar va)
      c2 <- forkAff $ attempt a2 >>= either (maybeKill va ve) (putVar va)
      (takeVar va) `cancelWith` (c1 <> c2)

instance plusPar :: Plus (Par e) where
  empty = Par empty

instance alternativePar :: Alternative (Par e)
