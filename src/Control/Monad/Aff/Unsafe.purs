module Control.Monad.Aff.Unsafe
  ( unsafeCoerceAff
  , module Control.Monad.Aff.Internal
  ) where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Internal (unsafeLaunchAff)
import Unsafe.Coerce (unsafeCoerce)

unsafeCoerceAff ∷ ∀ eff1 eff2 a. Aff eff1 a -> Aff eff2 a
unsafeCoerceAff = unsafeCoerce
