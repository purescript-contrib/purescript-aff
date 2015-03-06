module Examples where 
  import Debug.Trace(trace, Trace())

  import Data.Either(either)

  import Control.Monad.Aff
  import Control.Monad.Eff.Class(liftEff)
  import Control.Monad.Eff.Exception(error)
  import Control.Monad.Error.Class(throwError)
  
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

  test_attempt :: forall e. Aff (trace :: Trace | e) Unit
  test_attempt = do
    e <- attempt (throwError (error "Oh noes!"))
    liftEff $ either (const $ trace "Exception caught") (const $ trace "Exception NOT caught!!!") e

  main = launchAff $ do
    liftEff $ trace "Testing sequencing"
    test_sequencing 3

    liftEff $ trace "Testing attempt"
    test_attempt

    liftEff $ trace "Testing later"
    later $ liftEff $ trace "It happend later"

    
