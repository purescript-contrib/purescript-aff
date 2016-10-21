module Control.Monad.Aff.Unsafe where

import Control.Monad.Aff (Aff)
import Unsafe.Coerce (unsafeCoerce)

unsafeCoerceAff :: forall e1 e2 a. Aff e1 a -> Aff e2 a
unsafeCoerceAff = unsafeCoerce
