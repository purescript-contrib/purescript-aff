module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Parallel.Class (parallel, runParallel)
import Control.Monad.Aff (Aff, runAff, makeAff, later, later', forkAff, forkAll, Canceler(..), cancel, attempt, finally, apathize)
import Control.Monad.Aff.AVar (AVAR, makeVar, makeVar', putVar, modifyVar, takeVar, killVar)
import Control.Monad.Aff.Console (log)
import Control.Monad.Cont.Class (callCC)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, error, message)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Rec.Class (tailRecM)
import Data.Either (Either(..), either, fromLeft, fromRight)
import Data.Unfoldable (replicate)
import Partial.Unsafe (unsafePartial)

type Test a = forall e. Aff (console :: CONSOLE | e) a
type TestAVar a = forall e. Aff (console :: CONSOLE, avar :: AVAR | e) a

replicateArray :: forall a. Int -> a -> Array a
replicateArray = replicate

test_sequencing :: Int -> Test Unit
test_sequencing 0 = log "Done"
test_sequencing n = do
  later' 100 (log (show (n / 10) <> " seconds left"))
  test_sequencing (n - 1)

foreign import synchronousUnexpectedThrowError :: forall e. Eff e Unit

test_makeAff :: Test Unit
test_makeAff = unsafePartial do
  s <- attempt $ makeAff \reject resolve -> resolve "ok"
  log $ "makeAff success is " <> fromRight s

  asyncF <- attempt $ makeAff \reject resolve -> reject (error "ok")
  log $ "makeAff asynchronous failure is " <> message (fromLeft asyncF)

  asyncF' <- attempt $ makeAff \reject resolve -> synchronousUnexpectedThrowError
  log $ "makeAff synchronous failure is " <> message (fromLeft asyncF')

  log "Success: makeAff is ok"

test_pure :: Test Unit
test_pure = do
  pure unit
  pure unit
  pure unit
  log "Success: Got all the way past 4 pures"

test_attempt :: Test Unit
test_attempt = do
  e <- attempt (throwError (error "Oh noes!"))
  either (const $ log "Success: Exception caught") (const $ log "Failure: Exception NOT caught!!!") e

test_apathize :: Test Unit
test_apathize = do
  apathize $ throwError (error "Oh noes!")
  log "Success: Exceptions don't stop the apathetic"

test_putTakeVar :: TestAVar Unit
test_putTakeVar = do
  v <- makeVar
  forkAff (later $ putVar v 1.0)
  a <- takeVar v
  log ("Success: Value " <> show a)

test_killFirstForked :: Test Unit
test_killFirstForked = do
  c <- forkAff (later' 100 $ pure "Failure: This should have been killed!")
  b <- c `cancel` (error "Just die")
  log (if b then "Success: Killed first forked" else "Failure: Couldn't kill first forked")

test_killVar :: TestAVar Unit
test_killVar = do
  v <- makeVar
  killVar v (error "DOA")
  e <- attempt $ takeVar v
  either (const $ log "Success: Killed queue dead") (const $ log "Failure: Oh noes, queue survived!") e

test_finally :: TestAVar Unit
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
  log $ if n1 + n2 + n3 == 42 then "Success: effects amount to 42."
                                else "Failure: Expected 42."

test_parRace :: TestAVar Unit
test_parRace = do
  s <- runParallel (parallel (later' 100 $ pure "Success: Early bird got the worm") <|>
               parallel (later' 200 $ pure "Failure: Late bird got the worm"))
  log s

test_parError :: TestAVar Unit
test_parError = do
  e <- attempt $ runParallel (parallel (throwError (error ("Oh noes!"))) *> pure unit)
  either (const $ log "Success: Exception propagated") (const $ log "Failure: Exception missing") e

test_parRaceKill1 :: TestAVar Unit
test_parRaceKill1 = do
  s <- runParallel (parallel (later' 100 $ throwError (error ("Oh noes!"))) <|>
               parallel (later' 200 $ pure "Success: Early error was ignored in favor of late success"))
  log s

test_parRaceKill2 :: TestAVar Unit
test_parRaceKill2 = do
  e <- attempt $ runParallel (parallel (later' 100 $ throwError (error ("Oh noes!"))) <|>
                         parallel (later' 200 $ throwError (error ("Oh noes!"))))
  either (const $ log "Success: Killing both kills it dead") (const $ log "Failure: It's alive!!!") e

test_semigroupCanceler :: Test Unit
test_semigroupCanceler =
  let
    c = Canceler (const (pure true)) <> Canceler (const (pure true))
  in do
    v <- cancel c (error "CANCEL")
    log (if v then "Success: Canceled semigroup composite canceler"
                     else "Failure: Could not cancel semigroup composite canceler")

test_cancelLater :: TestAVar Unit
test_cancelLater = do
  c <- forkAff $ (do pure "Binding"
                     _ <- later' 100 $ log ("Failure: Later was not canceled!")
                     pure "Binding")
  v <- cancel c (error "Cause")
  log (if v then "Success: Canceled later" else "Failure: Did not cancel later")

test_cancelParallel :: TestAVar Unit
test_cancelParallel = do
  c  <- forkAff <<< runParallel $ parallel (later' 100 $ log "Failure: #1 should not get through") <|>
                             parallel (later' 100 $ log "Failure: #2 should not get through")
  v  <- c `cancel` (error "Must cancel")
  log (if v then "Success: Canceling composite of two Parallel succeeded"
                   else "Failure: Canceling composite of two Parallel failed")

test_syncTailRecM :: TestAVar Unit
test_syncTailRecM = do
  v <- makeVar' false
  _ <- forkAff $ tailRecM go { n: 1000000, v }
  b <- takeVar v
  log (if b then "Success: Synchronous tailRecM resolved synchronously"
            else "Failure: Synchronous tailRecM resolved asynchronously")
  where
  go { n: 0, v } = do
    modifyVar (const true) v
    pure (Right 0)
  go { n, v } = pure (Left { n: n - 1, v })

loopAndBounce :: forall eff. Int -> Aff (console :: CONSOLE | eff) Unit
loopAndBounce n = do
  res <- tailRecM go n
  log $ "Done: " <> show res
  where
  go 0 = pure (Right 0)
  go n | mod n 30000 == 0 = do
    later' 10 (pure unit)
    pure (Left (n - 1))
  go n = pure (Left (n - 1))

all :: forall eff. Int -> Aff (console :: CONSOLE, avar :: AVAR | eff) Unit
all n = do
  var <- makeVar' 0
  forkAll $ replicateArray n (modifyVar (_ + 1) var)
  count <- takeVar var
  log ("Forked " <> show count)

cancelAll :: forall eff. Int -> Aff (console :: CONSOLE, avar :: AVAR | eff) Unit
cancelAll n = do
  canceler <- forkAll $ replicateArray n (later' 100000 (log "oops"))
  canceled <- cancel canceler (error "bye")
  log ("Cancelled all: " <> show canceled)

delay :: forall eff. Int -> Aff eff Unit
delay n = callCC \cont ->
  later' n (cont unit)

main :: Eff (console :: CONSOLE, avar :: AVAR, err :: EXCEPTION) Unit
main = runAff throwException (const (pure unit)) $ do
  log "Testing sequencing"
  test_sequencing 3

  log "Testing pure"
  test_pure

  log "Testing makeAff"
  test_makeAff

  log "Testing attempt"
  test_attempt

  log "Testing later"
  later $ log "Success: It happened later"

  log "Testing kill of later"
  test_cancelLater

  log "Testing kill of first forked"
  test_killFirstForked

  log "Testing apathize"
  test_apathize

  log "Testing semigroup canceler"
  test_semigroupCanceler

  log "Testing AVar - putVar, takeVar"
  test_putTakeVar

  log "Testing AVar killVar"
  test_killVar

  log "Testing finally"
  test_finally

  log "Test Parallel (*>)"
  test_parError

  log "Testing Parallel (<|>)"
  test_parRace

  log "Testing Parallel (<|>) - kill one"
  test_parRaceKill1

  log "Testing Parallel (<|>) - kill two"
  test_parRaceKill2

  log "Testing cancel of Parallel (<|>)"
  test_cancelParallel

  log "Testing synchronous tailRecM"
  test_syncTailRecM

  log "pre-delay"
  delay 1000
  log "post-delay"

  loopAndBounce 1000000

  all 100000

  cancelAll 100000

  log "Done testing"
