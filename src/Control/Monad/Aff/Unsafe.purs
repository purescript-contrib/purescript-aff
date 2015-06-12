module Control.Monad.Aff.Unsafe where
  import Prelude (Unit())
  import Control.Monad.Aff

  foreign import unsafeTrace :: forall e a. a -> Aff e Unit

  foreign import unsafeInterleaveAff :: forall e1 e2 a. Aff e1 a -> Aff e2 a
