## Module Control.Monad.Aff.AVar

A low-level primitive for building asynchronous code.

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


