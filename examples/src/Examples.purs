module Examples where 
  import Debug.Trace(trace, Trace())

  import Data.Either(either)

  import Control.Monad.Aff
  import Control.Monad.Aff.Var
  import Control.Monad.Aff.Par
  import Control.Apply((*>))
  import Control.Alt(Alt, (<|>))
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

  type Test = forall e. Aff (trace :: Trace | e) Unit
  type TestVar = forall e. Aff (trace :: Trace, var :: VarFx | e) Unit
  type TestVarTime = forall e. Aff (trace :: Trace, var :: VarFx, time :: Time | e) Unit

  test_sequencing :: forall e. Number -> Aff (trace :: Trace, time :: Time | e) Unit
  test_sequencing 0 = liftEff $ trace "Done"
  test_sequencing n = do
    timeout 1000
    liftEff $ trace (show n ++ " seconds left")
    test_sequencing (n - 1)

  test_pure :: Test
  test_pure = do
    pure unit 
    pure unit
    pure unit
    liftEff $ trace "Success: Got all the way past 4 pures"

  test_attempt :: Test
  test_attempt = do
    e <- attempt (throwError (error "Oh noes!"))
    liftEff $ either (const $ trace "Success: Exception caught") (const $ trace "Failure: Exception NOT caught!!!") e

  test_apathize :: Test
  test_apathize = do
    apathize $ throwError (error "Oh noes!")
    liftEff $ trace "Success: Didn't care about return value"

  test_putTakeVar :: TestVar
  test_putTakeVar = do
    v <- makeVar
    forkAff (later $ putVar v 1.0)
    a <- takeVar v 
    liftEff $ trace ("Success: Value " ++ show a)

  test_killVar :: TestVar
  test_killVar = do
    v <- makeVar
    killVar v (error "DOA")
    e <- attempt $ takeVar v
    liftEff $ either (const $ trace "Success: Killed var dead") (const $ trace "Failure: Oh noes, Var survived!") e

  test_parRace :: TestVarTime
  test_parRace = do
    s <- runPar $ (Par (timeout  100 *> pure "Success: Early bird got the worm") <|> 
                   Par (timeout 1000 *> pure "Failure: Late bird got the worm"))
    liftEff $ trace s

  main = launchAff $ do
    liftEff $ trace "Testing sequencing"
    test_sequencing 3

    liftEff $ trace "Testing pure"
    test_pure

    liftEff $ trace "Testing attempt"
    test_attempt

    liftEff $ trace "Testing later"
    later $ liftEff $ trace "Success: It happened later"

    liftEff $ trace "Testing apathize"
    test_apathize

    liftEff $ trace "Testing Var - putVar, takeVar"
    test_putTakeVar

    liftEff $ trace "Testing killVar"
    test_killVar

    liftEff $ trace "Testing Par (<|>)"
    test_parRace
