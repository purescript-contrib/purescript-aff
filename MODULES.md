# Module Documentation

## Module Control.Monad.Aff

#### `Async`

``` purescript
data Async :: !
```

The effect of being asynchronous.

#### `EffA`

``` purescript
type EffA e a = Eff (async :: Async | e) a
```

The `Eff` type for a computation which has asynchronous effects.

#### `Aff`

``` purescript
data Aff e a
```

A computation with effects `e`. The computation either errors or 
produces a value of type `a`.

This is moral equivalent of `ErrorT (ContT Unit (EffA e)) a`.

The type implements `MonadEff` so it's easy to lift synchronous `Eff` 
computations into this type. As a result, there's basically no reason to
use `Eff` in a program that has some asynchronous computations.

#### `PureAff`

``` purescript
type PureAff a = forall e. Aff e a
```


#### `launchAff`

``` purescript
launchAff :: forall e a. Aff e a -> EffA e Unit
```

Converts the asynchronous effect into a synchronous one. All values and
errors are ignored.

#### `runAff`

``` purescript
runAff :: forall e a. (Error -> Eff e Unit) -> (a -> Eff e Unit) -> Aff e a -> EffA e Unit
```

Runs the asynchronous effect. You must supply an error callback and a 
success callback.

#### `makeAff`

``` purescript
makeAff :: forall e a. ((Error -> Eff e Unit) -> (a -> Eff e Unit) -> EffA e Unit) -> Aff e a
```

Creates an asynchronous effect from a function that accepts error and 
success callbacks.

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



## Module Control.Monad.Aff.Unsafe

#### `unsafeInterleaveAff`

``` purescript
unsafeInterleaveAff :: forall e1 e2 a. Aff e1 a -> Aff e2 a
```



## Module Control.Monad.Aff.Var

#### `VarF`

``` purescript
data VarF :: !
```


#### `Var`

``` purescript
data Var :: * -> *
```


#### `AffVar`

``` purescript
type AffVar e a = Aff (var :: VarF | e) a
```


#### `makeVar`

``` purescript
makeVar :: forall e a. AffVar e (Var a)
```

Makes a new asynchronous variable.

#### `takeVar`

``` purescript
takeVar :: forall e a. Var a -> AffVar e a
```

Takes the next value from the asynchronous variable.

#### `putVar`

``` purescript
putVar :: forall e a. Var a -> a -> AffVar e Unit
```

Puts a new value into the asynchronous variable. If the variable has
been killed, this will result in an error.

#### `killVar`

``` purescript
killVar :: forall e a. Var a -> Error -> AffVar e Unit
```

Kills an asynchronous variable.