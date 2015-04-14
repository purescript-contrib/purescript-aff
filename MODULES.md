# Module Documentation

## Module Control.Monad.Aff

#### `Aff`

``` purescript
data Aff :: # ! -> * -> *
```

A computation with effects `e`. The computation either errors or
produces a value of type `a`.

This is moral equivalent of `ErrorT (ContT Unit (Eff e)) a`.

#### `PureAff`

``` purescript
type PureAff a = forall e. Aff e a
```

A pure asynchronous computation, having no effects.

#### `Canceler`

``` purescript
newtype Canceler e
  = Canceler (Error -> Aff e Boolean)
```

A canceler is asynchronous function that can be used to attempt the 
cancelation of a computation. Returns a boolean flag indicating whether
or not the cancellation was successful.

#### `cancel`

``` purescript
cancel :: forall e. Canceler e -> Error -> Aff e Boolean
```

Unwraps the canceler function from the newtype that wraps it.

#### `cancelWith`

``` purescript
cancelWith :: forall e a. Aff e a -> Canceler e -> Aff e a
```

This function allows you to attach a custom canceler to an asynchronous
computation. If the computation is canceled, then the custom canceler 
will be run along side the computation's own canceler.

#### `launchAff`

``` purescript
launchAff :: forall e a. Aff e a -> Eff e Unit
```

Converts the asynchronous computation into a synchronous one. All values
and errors are ignored.

#### `runAff`

``` purescript
runAff :: forall e a. (Error -> Eff e Unit) -> (a -> Eff e Unit) -> Aff e a -> Eff e Unit
```

Runs the asynchronous computation. You must supply an error callback and a
success callback.

#### `makeAff`

``` purescript
makeAff :: forall e a. ((Error -> Eff e Unit) -> (a -> Eff e Unit) -> Eff e Unit) -> Aff e a
```

Creates an asynchronous effect from a function that accepts error and
success callbacks. This function can be used for asynchronous computations
that cannot be canceled.

#### `makeAff'`

``` purescript
makeAff' :: forall e a. ((Error -> Eff e Unit) -> (a -> Eff e Unit) -> Eff e (Canceler e)) -> Aff e a
```

Creates an asynchronous effect from a function that accepts error and
success callbacks, and returns a canceler for the computation. This
function can be used for asynchronous computations that can be canceled.

#### `later`

``` purescript
later :: forall e a. Aff e a -> Aff e a
```

Runs the asynchronous computation off the current execution context.

#### `later'`

``` purescript
later' :: forall e a. Number -> Aff e a -> Aff e a
```

Runs the asynchronous computation later (off the current execution context).

#### `forkAff`

``` purescript
forkAff :: forall e a. Aff e a -> Aff e (Canceler e)
```

Forks the specified asynchronous computation so subsequent monadic binds
will not block on the result of the computation.

#### `attempt`

``` purescript
attempt :: forall e a. Aff e a -> Aff e (Either Error a)
```

Promotes any error to the value level of the asynchronous monad.

#### `apathize`

``` purescript
apathize :: forall e a. Aff e a -> Aff e Unit
```

Ignores any errors.

#### `liftEff'`

``` purescript
liftEff' :: forall e a. Eff (err :: Exception | e) a -> Aff e (Either Error a)
```

Lifts a synchronous computation and makes explicit any failure from exceptions.

#### `nonCanceler`

``` purescript
nonCanceler :: forall e. Canceler e
```

A constant function that always returns a pure false value.

#### `semigroupAff`

``` purescript
instance semigroupAff :: (Semigroup a) => Semigroup (Aff e a)
```


#### `monoidAff`

``` purescript
instance monoidAff :: (Monoid a) => Monoid (Aff e a)
```


#### `functorAff`

``` purescript
instance functorAff :: Functor (Aff e)
```


#### `applyAff`

``` purescript
instance applyAff :: Apply (Aff e)
```


#### `applicativeAff`

``` purescript
instance applicativeAff :: Applicative (Aff e)
```


#### `bindAff`

``` purescript
instance bindAff :: Bind (Aff e)
```


#### `monadAff`

``` purescript
instance monadAff :: Monad (Aff e)
```


#### `monadEffAff`

``` purescript
instance monadEffAff :: MonadEff e (Aff e)
```


#### `monadErrorAff`

``` purescript
instance monadErrorAff :: MonadError Error (Aff e)
```

Allows users to catch and throw errors on the error channel of the
asynchronous computation. See documentation in `purescript-transformers`.

#### `altAff`

``` purescript
instance altAff :: Alt (Aff e)
```


#### `plusAff`

``` purescript
instance plusAff :: Plus (Aff e)
```


#### `alternativeAff`

``` purescript
instance alternativeAff :: Alternative (Aff e)
```


#### `monadPlusAff`

``` purescript
instance monadPlusAff :: MonadPlus (Aff e)
```


#### `semigroupCanceler`

``` purescript
instance semigroupCanceler :: Semigroup (Canceler e)
```


#### `monoidCanceler`

``` purescript
instance monoidCanceler :: Monoid (Canceler e)
```



## Module Control.Monad.Aff.AVar

#### `AVAR`

``` purescript
data AVAR :: !
```


#### `AVar`

``` purescript
data AVar :: * -> *
```


#### `AffAVar`

``` purescript
type AffAVar e a = Aff (avar :: AVAR | e) a
```


#### `makeVar`

``` purescript
makeVar :: forall e a. AffAVar e (AVar a)
```

Makes a new asynchronous avar.

#### `makeVar'`

``` purescript
makeVar' :: forall e a. a -> AffAVar e (AVar a)
```

Makes a avar and sets it to some value.

#### `takeVar`

``` purescript
takeVar :: forall e a. AVar a -> AffAVar e a
```

Takes the next value from the asynchronous avar.

#### `putVar`

``` purescript
putVar :: forall e a. AVar a -> a -> AffAVar e Unit
```

Puts a new value into the asynchronous avar. If the avar has
been killed, this will result in an error.

#### `modifyVar`

``` purescript
modifyVar :: forall e a. (a -> a) -> AVar a -> AffAVar e Unit
```

Modifies the value at the head of the avar (will suspend until one is available).

#### `killVar`

``` purescript
killVar :: forall e a. AVar a -> Error -> AffAVar e Unit
```

Kills an asynchronous avar.


## Module Control.Monad.Aff.Class

#### `MonadAff`

``` purescript
class MonadAff e m where
  liftAff :: forall a. Aff e a -> m a
```


#### `monadAffAff`

``` purescript
instance monadAffAff :: MonadAff e (Aff e)
```



## Module Control.Monad.Aff.Par

#### `Par`

``` purescript
newtype Par e a
  = Par (AffAVar e a)
```


#### `runPar`

``` purescript
runPar :: forall e a. Par e a -> AffAVar e a
```

Extracts the `Aff` from the `Par`.

#### `semigroupPar`

``` purescript
instance semigroupPar :: (Semigroup a) => Semigroup (Par e a)
```


#### `monoidPar`

``` purescript
instance monoidPar :: (Monoid a) => Monoid (Par e a)
```


#### `functorPar`

``` purescript
instance functorPar :: Functor (Par e)
```


#### `applyPar`

``` purescript
instance applyPar :: Apply (Par e)
```


#### `applicativePar`

``` purescript
instance applicativePar :: Applicative (Par e)
```


#### `altPar`

``` purescript
instance altPar :: Alt (Par e)
```

Returns the first value, or the first error if both error.

#### `plusPar`

``` purescript
instance plusPar :: Plus (Par e)
```


#### `alternativePar`

``` purescript
instance alternativePar :: Alternative (Par e)
```



## Module Control.Monad.Aff.Unsafe

#### `unsafeTrace`

``` purescript
unsafeTrace :: forall e a. a -> Aff e Unit
```


#### `unsafeInterleaveAff`

``` purescript
unsafeInterleaveAff :: forall e1 e2 a. Aff e1 a -> Aff e2 a
```