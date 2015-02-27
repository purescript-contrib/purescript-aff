# Module Documentation

## Module Control.Monad.Aff

#### `Aff`

``` purescript
data Aff e a
```


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