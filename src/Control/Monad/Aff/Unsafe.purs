module Control.Monad.Aff.Unsafe where
  import Control.Monad.Aff

  foreign import unsafeTrace """
    function unsafeTrace(v) {
      return function(success, error) {
        console.log(v);

        try {
          success(v);
        } catch (e) {
          error(e);
        }

        var nonCanceler;

        nonCanceler = function(e) {
          return function(sucess, error) {
            success(false);

            return nonCanceler;
          }
        };

        return nonCanceler;
      };
    }
  """ :: forall e a. a -> Aff e Unit

  foreign import unsafeInterleaveAff """
    function unsafeInterleaveAff(aff) {
      return aff;
    }
  """ :: forall e1 e2 a. Aff e1 a -> Aff e2 a
