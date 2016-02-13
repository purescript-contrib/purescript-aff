## Module Control.Monad.Aff.Console

#### `log`

``` purescript
log :: forall e. String -> Aff (console :: CONSOLE | e) Unit
```

Logs any string to the console. This basically saves you
from writing `liftEff $ log x` everywhere.

#### `print`

``` purescript
print :: forall e a. (Show a) => a -> Aff (console :: CONSOLE | e) Unit
```

Prints any `Show`-able value to the console. This basically saves you
from writing `liftEff $ print x` everywhere.


