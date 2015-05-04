module Examples where 
  import Debug.Trace(Trace())

  import Data.Either(either)

  import Control.Monad.Aff
  import Control.Monad.Aff.AVar
  import Control.Monad.Aff.Par
  import Control.Monad.Aff.Debug.Trace(trace)
  import Control.Apply((*>))
  import Control.Alt(Alt, (<|>))
  import Control.Monad.Eff.Class(liftEff)
  import Control.Monad.Eff.Exception(error)
  import Control.Monad.Error.Class(throwError)
  
  type Test a = forall e. Aff (trace :: Trace | e) a
  type TestAVar a = forall e. Aff (trace :: Trace, avar :: AVAR | e) a

  test_sequencing :: Number -> Test _
  test_sequencing 0 = trace "Done"
  test_sequencing n = do
    later' 100 (trace (show (n / 10) ++ " seconds left"))
    test_sequencing (n - 1)

  test_pure :: Test _
  test_pure = do
    pure unit 
    pure unit
    pure unit
    trace "Success: Got all the way past 4 pures"

  test_attempt :: Test _
  test_attempt = do
    e <- attempt (throwError (error "Oh noes!"))
    either (const $ trace "Success: Exception caught") (const $ trace "Failure: Exception NOT caught!!!") e

  test_apathize :: Test _
  test_apathize = do
    apathize $ throwError (error "Oh noes!")
    trace "Success: Exceptions don't stop the apathetic"

  test_putTakeVar :: TestAVar _
  test_putTakeVar = do
    v <- makeVar
    forkAff (later $ putVar v 1.0)
    a <- takeVar v 
    trace ("Success: Value " ++ show a)

  test_killFirstForked :: Test _
  test_killFirstForked = do 
    c <- forkAff (later' 100 $ pure "Failure: This should have been killed!")
    b <- c `cancel` (error "Just die")
    trace (if b then "Success: Killed first forked" else "Failure: Couldn't kill first forked")


  test_killVar :: TestAVar _
  test_killVar = do
    v <- makeVar
    killVar v (error "DOA")
    e <- attempt $ takeVar v
    either (const $ trace "Success: Killed queue dead") (const $ trace "Failure: Oh noes, queue survived!") e

  test_finally :: TestAVar _
  test_finally = do
    v <- makeVar
    finally
      (putVar v 0)
      (putVar v 2)
    apathize $ finally
      (throwError (error "poof!") *> putVar v 666) -- this putVar should not get executed
      (putVar v 40)
    n1 <- takeVar v
    n2 <- takeVar v
    n3 <- takeVar v
    trace $ if n1 + n2 + n3 == 42 then "Success: effects amount to 42."
                                  else "Failure: Expected 42."

  test_parRace :: TestAVar _
  test_parRace = do
    s <- runPar (Par (later' 100 $ pure "Success: Early bird got the worm") <|> 
                 Par (later' 200 $ pure "Failure: Late bird got the worm"))
    trace s

  test_parRaceKill1 :: TestAVar _
  test_parRaceKill1 = do
    s <- runPar (Par (later' 100 $ throwError (error ("Oh noes!"))) <|> 
                 Par (later' 200 $ pure "Success: Early error was ignored in favor of late success"))
    trace s

  test_parRaceKill2 :: TestAVar _
  test_parRaceKill2 = do
    e <- attempt $ runPar (Par (later' 100 $ throwError (error ("Oh noes!"))) <|> 
                           Par (later' 200 $ throwError (error ("Oh noes!"))))
    either (const $ trace "Success: Killing both kills it dead") (const $ trace "Failure: It's alive!!!") e

  test_semigroupCanceler :: Test _
  test_semigroupCanceler = 
    let 
      c = Canceler (const (pure true)) <> Canceler (const (pure true))
    in do 
      v <- cancel c (error "CANCEL")
      trace (if v then "Success: Canceled semigroup composite canceler" 
                       else "Failure: Could not cancel semigroup composite canceler")

  test_cancelLater :: TestAVar _
  test_cancelLater = do
    c <- forkAff $ (do pure "Binding"
                       _ <- later' 100 $ trace ("Failure: Later was not canceled!")
                       pure "Binding")
    v <- cancel c (error "Cause")
    trace (if v then "Success: Canceled later" else "Failure: Did not cancel later")

  test_cancelPar :: TestAVar _
  test_cancelPar = do
    c  <- forkAff <<< runPar $ Par (later' 100 $ trace "Failure: #1 should not get through") <|>
                               Par (later' 100 $ trace "Failure: #2 should not get through")
    v  <- c `cancel` (error "Must cancel")
    trace (if v then "Success: Canceling composite of two Par succeeded" 
                     else "Failure: Canceling composite of two Par failed")

  main = launchAff $ do
    trace "Testing sequencing"
    test_sequencing 3

    trace "Testing pure"
    test_pure

    trace "Testing attempt"
    test_attempt

    trace "Testing later"
    later $ trace "Success: It happened later"

    trace "Testing kill of later"
    test_cancelLater

    trace "Testing kill of first forked"
    test_killFirstForked

    trace "Testing apathize"
    test_apathize

    trace "Testing semigroup canceler"
    test_semigroupCanceler

    trace "Testing AVar - putVar, takeVar"
    test_putTakeVar

    trace "Testing AVar killVar"
    test_killVar

    trace "Testing finally"
    test_finally

    trace "Testing Par (<|>)"
    test_parRace

    trace "Testing Par (<|>) - kill one"
    test_parRaceKill1

    trace "Testing Par (<|>) - kill two"
    test_parRaceKill2

    trace "Testing cancel of Par (<|>)"
    test_cancelPar

    trace "Done testing"
