module Examples where 
  import Debug.Trace(trace, Trace())

  import Data.Either(either)

  import Control.Monad.Aff
  import Control.Monad.Aff.Queue
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
  type TestQueue = forall e. Aff (trace :: Trace, queue :: QueueFx | e) Unit
  type TestQueueTime = forall e. Aff (trace :: Trace, queue :: QueueFx, time :: Time | e) Unit

  test_sequencing :: forall e. Number -> Aff (trace :: Trace, time :: Time | e) Unit
  test_sequencing 0 = liftEff $ trace "Done"
  test_sequencing n = do
    timeout 100
    liftEff $ trace (show (n / 10) ++ " seconds left")
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
    liftEff $ trace "Success: Exceptions don't stop the apathetic"

  test_putTakeQueue :: TestQueue
  test_putTakeQueue = do
    v <- makeQueue
    forkAff (later $ putQueue v 1.0)
    a <- takeQueue v 
    liftEff $ trace ("Success: Value " ++ show a)

  test_killQueue :: TestQueue
  test_killQueue = do
    v <- makeQueue
    killQueue v (error "DOA")
    e <- attempt $ takeQueue v
    liftEff $ either (const $ trace "Success: Killed queue dead") (const $ trace "Failure: Oh noes, queue survived!") e

  test_parRace :: TestQueueTime
  test_parRace = do
    s <- runPar (Par (timeout 100 *> pure "Success: Early bird got the worm") <|> 
                 Par (timeout 200 *> pure "Failure: Late bird got the worm"))
    liftEff $ trace s

  test_parRaceKill1 :: TestQueueTime
  test_parRaceKill1 = do
    s <- runPar (Par (timeout 100 *> throwError (error ("Oh noes!"))) <|> 
                 Par (timeout 200 *> pure "Success: Early error was ignored in favor of late success"))
    liftEff $ trace s

  test_parRaceKill2 :: TestQueueTime
  test_parRaceKill2 = do
    e <- attempt $ runPar (Par (timeout 100 *> throwError (error ("Oh noes!"))) <|> 
                           Par (timeout 200 *> throwError (error ("Oh noes!"))))
    liftEff $ either (const $ trace "Success: Killing both kills it dead") (const $ trace "Failure: It's alive!!!") e

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

    liftEff $ trace "Testing Queue - putQueue, takeQueue"
    test_putTakeQueue

    liftEff $ trace "Testing killQueue"
    test_killQueue

    liftEff $ trace "Testing Par (<|>)"
    test_parRace

    liftEff $ trace "Testing Par (<|>) - kill one"
    test_parRaceKill1

    liftEff $ trace "Testing Par (<|>) - kill two"
    test_parRaceKill2
