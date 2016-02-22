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
(Semigroup a) => Semigroup (Par e a)
(Monoid a) => Monoid (Par e a)
Functor (Par e)
Apply (Par e)
Applicative (Par e)
Alt (Par e)
Plus (Par e)
Alternative (Par e)
```

#### `runPar`

``` purescript
runPar :: forall e a. Par e a -> AffAVar e a
```

Extracts the `Aff` from the `Par`.


