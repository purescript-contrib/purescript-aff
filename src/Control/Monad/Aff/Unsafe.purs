module Control.Monad.Aff.Unsafe
  ( unsafeCoerceAff
  , module Internal
  ) where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Internal (unsafeLaunchAff, unsafeLiftEff, unsafeMakeAff) as Internal
import Unsafe.Coerce (unsafeCoerce)

unsafeCoerceAff ∷ ∀ eff1 eff2 a. Aff eff1 a -> Aff eff2 a
unsafeCoerceAff = unsafeCoerce
