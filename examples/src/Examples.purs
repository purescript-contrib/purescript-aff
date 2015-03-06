module Examples where 
  import Control.Monad.Aff
  import Control.Monad.Eff.Class(liftEff)
  import Debug.Trace

  foreign import data Time :: !

  foreign import timeout """
    function timeout(time) {
      return function(error) {
        return function(success) {
          return function() {
            setTimeout(function() {
              try {
                success({})();
              } catch (e) {
                error(e)();
              }
            }, time);
          }
        }
      }
    }
  """ :: forall e. Number -> Aff (time :: Time | e) Unit

  test_sequencing 0 = liftEff $ trace "Done"
  test_sequencing n = do
    timeout 1000
    liftEff $ trace (show n ++ " seconds left")
    test_sequencing (n - 1)

  main = launchAff $ do
    liftEff $ trace "Testing sequencing"
    test_sequencing 10
