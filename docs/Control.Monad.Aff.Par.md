## Module Control.Monad.Aff.Par

A newtype over `Aff` that provides `Applicative` instances that run in 
parallel. This is useful, for example, if you want to run a whole bunch 
of AJAX requests at the same time, rather than sequentially.

#### `Par`

``` purescript
newtype Par e a
  = Par (AffAVar e a)
```

##### Instances
``` purescript
instance semigroupPar :: (Semigroup a) => Semigroup (Par e a)
instance monoidPar :: (Monoid a) => Monoid (Par e a)
instance functorPar :: Functor (Par e)
instance applyPar :: Apply (Par e)
instance applicativePar :: Applicative (Par e)
instance altPar :: Alt (Par e)
instance plusPar :: Plus (Par e)
instance alternativePar :: Alternative (Par e)
```

#### `runPar`

``` purescript
runPar :: forall e a. Par e a -> AffAVar e a
```

Extracts the `Aff` from the `Par`.


