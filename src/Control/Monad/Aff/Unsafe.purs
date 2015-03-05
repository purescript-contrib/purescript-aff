module Control.Monad.Aff.Unsafe where
  import Control.Monad.Aff

  foreign import unsafeInterleaveAff """
    function unsafeInterleaveAff(aff) {
      return aff;
    }
  """ :: forall e1 e2 a. Aff e1 a -> Aff e2 a
