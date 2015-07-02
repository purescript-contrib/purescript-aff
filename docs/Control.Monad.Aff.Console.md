## Module Control.Monad.Aff.Console

#### `log`

``` purescript
log :: forall e. String -> Aff (console :: CONSOLE | e) String
```

Logs any string to the console. This basically saves you
from writing `liftEff $ log x` everywhere.

#### `print`

``` purescript
print :: forall e a. (Show a) => a -> Aff (console :: CONSOLE | e) a
```

Prints any `Show`-able value to the console. This basically saves you 
from writing `liftEff $ print x` everywhere.


