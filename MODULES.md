# Module Documentation

## Module Control.Monad.Aff

#### `Aff`

``` purescript
data Aff e a
```

An asynchronous computation with effects `e`, which either errors or 
produces a value of type `a`.

This is the moral equivalent of `ErrorT (ContT Unit (Eff e)) a`, but 
faster, easier to use, and self-contained.

The type implements `MonadEff` so it's easy to lift synchronous `Eff` 
computations into this type. As a result, there's basically no reason to
use `Eff` in a program that has some asynchronous computations.

#### `PureAff`

``` purescript
type PureAff a = forall e. Aff e a
```


#### `launchAff`

``` purescript
launchAff :: forall e a. Aff e a -> Eff e Unit
```

Converts the asynchronous effect into a synchronous one. All values and
errors are ignored.

#### `runAff`

``` purescript
runAff :: forall e a. (Error -> Eff e Unit) -> (a -> Eff e Unit) -> Aff e a -> Eff e Unit
```

Runs the asynchronous effect. You must supply an error callback and a 
success callback.

#### `makeAff`

``` purescript
makeAff :: forall e a. ((Error -> Eff e Unit) -> (a -> Eff e Unit) -> Eff e Unit) -> Aff e a
```

Creates an asynchronous effect from a function that accepts error and 
success callbacks.

#### `attempt`

``` purescript
attempt :: forall e a. Aff e a -> Aff e (Either Error a)
```

Promotes any error to the value level of the asynchronous monad.

#### `catch`

``` purescript
catch :: forall e a. Aff (err :: Exception | e) a -> Aff e a
```

Removes exceptions by forcing them through the error callback.

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