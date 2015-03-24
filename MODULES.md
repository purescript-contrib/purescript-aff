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
success callbacks.

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
forkAff :: forall e a. Aff e a -> Aff e Unit
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
  = Par (AffQueue e a)
```


#### `runPar`

``` purescript
runPar :: forall e a. Par e a -> AffQueue e a
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



## Module Control.Monad.Aff.Queue

#### `QueueFx`

``` purescript
data QueueFx :: !
```


#### `Queue`

``` purescript
data Queue :: * -> *
```


#### `AffQueue`

``` purescript
type AffQueue e a = Aff (queue :: QueueFx | e) a
```


#### `makeQueue`

``` purescript
makeQueue :: forall e a. AffQueue e (Queue a)
```

Makes a new asynchronous queue.

#### `makeQueue'`

``` purescript
makeQueue' :: forall e a. a -> AffQueue e (Queue a)
```

Makes a queue and sets it to some value.

#### `takeQueue`

``` purescript
takeQueue :: forall e a. Queue a -> AffQueue e a
```

Takes the next value from the asynchronous queue.

#### `putQueue`

``` purescript
putQueue :: forall e a. Queue a -> a -> AffQueue e Unit
```

Puts a new value into the asynchronous queue. If the queue has
been killed, this will result in an error.

#### `modifyQueue`

``` purescript
modifyQueue :: forall e a. (a -> a) -> Queue a -> AffQueue e Unit
```

Modifies the value at the head of the queue (will suspend until one is available).

#### `killQueue`

``` purescript
killQueue :: forall e a. Queue a -> Error -> AffQueue e Unit
```

Kills an asynchronous queue.


## Module Control.Monad.Aff.Unsafe

#### `unsafeTrace`

``` purescript
unsafeTrace :: forall e a. a -> Aff e Unit
```


#### `unsafeInterleaveAff`

``` purescript
unsafeInterleaveAff :: forall e1 e2 a. Aff e1 a -> Aff e2 a
```