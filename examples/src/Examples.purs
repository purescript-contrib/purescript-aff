module Examples where 
  import Debug.Trace(trace, Trace())

  import Data.Either(either)

  import Control.Monad.Aff
  import Control.Monad.Aff.AVar
  import Control.Monad.Aff.Par
  import Control.Apply((*>))
  import Control.Alt(Alt, (<|>))
  import Control.Monad.Eff.Class(liftEff)
  import Control.Monad.Eff.Exception(error)
  import Control.Monad.Error.Class(throwError)
  
  type Test = forall e. Aff (trace :: Trace | e) Unit
  type TestAVar = forall e. Aff (trace :: Trace, avar :: AVAR | e) Unit

  test_sequencing :: Number -> Test
  test_sequencing 0 = liftEff $ trace "Done"
  test_sequencing n = do
    later' 100 (liftEff $ trace (show (n / 10) ++ " seconds left"))
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

  test_putTakeVar :: TestAVar
  test_putTakeVar = do
    v <- makeVar
    forkAff (later $ putVar v 1.0)
    a <- takeVar v 
    liftEff $ trace ("Success: Value " ++ show a)

  test_killFirstForked :: Test
  test_killFirstForked = do 
    c <- forkAff (later' 100 $ pure "Failure: This should have been killed!")
    b <- c `cancel` (error "Just die")
    liftEff $ trace (if b then "Success: Killed first forked" else "Failure: Couldn't kill first forked")


  test_killVar :: TestAVar
  test_killVar = do
    v <- makeVar
    killVar v (error "DOA")
    e <- attempt $ takeVar v
    liftEff $ either (const $ trace "Success: Killed queue dead") (const $ trace "Failure: Oh noes, queue survived!") e

  test_parRace :: TestAVar
  test_parRace = do
    s <- runPar (Par (later' 100 $ pure "Success: Early bird got the worm") <|> 
                 Par (later' 200 $ pure "Failure: Late bird got the worm"))
    liftEff $ trace s

  test_parRaceKill1 :: TestAVar
  test_parRaceKill1 = do
    s <- runPar (Par (later' 100 $ throwError (error ("Oh noes!"))) <|> 
                 Par (later' 200 $ pure "Success: Early error was ignored in favor of late success"))
    liftEff $ trace s

  test_parRaceKill2 :: TestAVar
  test_parRaceKill2 = do
    e <- attempt $ runPar (Par (later' 100 $ throwError (error ("Oh noes!"))) <|> 
                           Par (later' 200 $ throwError (error ("Oh noes!"))))
    liftEff $ either (const $ trace "Success: Killing both kills it dead") (const $ trace "Failure: It's alive!!!") e

  test_semigroupCanceler :: Test
  test_semigroupCanceler = 
    let 
      c = Canceler (const (pure true)) <> Canceler (const (pure true))
    in do 
      v <- cancel c (error "CANCEL")
      liftEff $ trace (if v then "Success: Canceled semigroup composite canceler" 
                       else "Failure: Could not cancel semigroup composite canceler")

  test_cancelLater :: TestAVar
  test_cancelLater = do
    c <- forkAff $ (do pure "Binding"
                       _ <- later' 100 $ liftEff $ trace ("Failure: Later was not canceled!")
                       pure "Binding")
    v <- cancel c (error "Cause")
    liftEff $ trace (if v then "Success: Canceled later" else "Failure: Did not cancel later")

  test_cancelPar :: TestAVar
  test_cancelPar = do
    c  <- forkAff <<< runPar $ Par (later' 100 $ liftEff $ trace "Failure: #1 should not get through") <|>
                               Par (later' 100 $ liftEff $ trace "Failure: #2 should not get through")
    v  <- c `cancel` (error "Must cancel")
    liftEff $ trace (if v then "Success: Canceling composite of two Par succeeded" 
                     else "Failure: Canceling composite of two Par failed")

  main = launchAff $ do
    liftEff $ trace "Testing sequencing"
    test_sequencing 3

    liftEff $ trace "Testing pure"
    test_pure

    liftEff $ trace "Testing attempt"
    test_attempt

    liftEff $ trace "Testing later"
    later $ liftEff $ trace "Success: It happened later"

    liftEff $ trace "Testing kill of later"
    test_cancelLater

    liftEff $ trace "Testing kill of first forked"
    test_killFirstForked

    liftEff $ trace "Testing apathize"
    test_apathize

    liftEff $ trace "Testing semigroup canceler"
    test_semigroupCanceler

    liftEff $ trace "Testing AVar - putVar, takeVar"
    test_putTakeVar

    liftEff $ trace "Testing AVar killVar"
    test_killVar

    liftEff $ trace "Testing Par (<|>)"
    test_parRace

    liftEff $ trace "Testing Par (<|>) - kill one"
    test_parRaceKill1

    liftEff $ trace "Testing Par (<|>) - kill two"
    test_parRaceKill2

    liftEff $ trace "Testing cancel of Par (<|>)"
    test_cancelPar

    liftEff $ trace "Done testing"
