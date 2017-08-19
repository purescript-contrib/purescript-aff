module Control.Monad.Aff.Unsafe
  ( unsafeCoerceAff
  ) where

import Control.Monad.Aff (Aff)
import Unsafe.Coerce (unsafeCoerce)

unsafeCoerceAff ∷ ∀ eff1 eff2 a. Aff eff1 a -> Aff eff2 a
unsafeCoerceAff = unsafeCoerce
