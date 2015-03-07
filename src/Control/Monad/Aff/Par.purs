-- | A newtype over `Aff` that provides `Applicative` instances that run in 
-- | parallel.
module Control.Monad.Aff.Par 
  ( Par(..)
  , runPar
  ) where

  import Control.Monad.Aff
  import Control.Monad.Aff.Var

  import Data.Either(Either(..), either)
  import Data.Monoid(Monoid, mempty)

  import Control.Apply
  import Control.Alt(Alt, (<|>))
  import Control.Plus(Plus, empty)
  import Control.Alternative(Alternative)
  import Control.Monad.Error.Class(throwError, catchError)
  
  newtype Par e a = Par (AffVar e a)

  -- | Extracts the `Aff` from the `Par`.
  runPar :: forall e a. Par e a -> AffVar e a
  runPar (Par aff) = aff

  instance semigroupPar :: (Semigroup a) => Semigroup (Par e a) where
    (<>) a b = (<>) <$> a <*> b

  instance monoidPar :: (Monoid a) => Monoid (Par e a) where
    mempty = pure mempty

  instance functorPar :: Functor (Par e) where
    (<$>) f (Par fa) = Par (f <$> fa)

  instance applyPar :: Apply (Par e) where
    (<*>) (Par ff) (Par fa) = Par do
      vf <- makeVar 
      va <- makeVar
      forkAff (ff >>= putVar vf)
      forkAff (fa >>= putVar va)
      takeVar vf <*> takeVar va

  instance applicativePar :: Applicative (Par e) where
    pure v = Par (pure v)

  -- | Returns the first value, or the first error if both error.
  instance altPar :: Alt (Par e) where
    (<|>) (Par a1) (Par a2) = 
      let maybeKill va ve err = 
        do e <- takeVar ve
           if e == 1 then killVar va err else return unit
           putVar ve (e + 1)
      in Par do
        va <- makeVar     -- the `a` value
        ve <- makeVar' 0  -- the error count (starts at 0)
        forkAff $ attempt a1 >>= either (maybeKill va ve) (putVar va)
        forkAff $ attempt a2 >>= either (maybeKill va ve) (putVar va)
        takeVar va

  instance plusPar :: Plus (Par e) where
    empty = Par empty

  instance alternativePar :: Alternative (Par e)



