module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Monad.Aff (Aff, Canceler(..), runAff_, launchAff, makeAff, try, bracket, generalBracket, delay, forkAff, suspendAff, joinFiber, killFiber, never, supervise, Error, error, message)
import Control.Monad.Aff.AVar (AVAR, makeEmptyVar, takeVar, putVar)
import Control.Monad.Aff.Compat as AC
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Exception (throwException, EXCEPTION)
import Control.Monad.Eff.Ref (REF, Ref)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Eff.Ref.Unsafe (unsafeRunRef)
import Control.Monad.Eff.Timer (TIMER, setTimeout, clearTimeout)
import Control.Monad.Error.Class (throwError, catchError)
import Control.Parallel (parallel, sequential, parTraverse_)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, isLeft, isRight)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Test.Assert (assert', ASSERT)

type TestEffects eff = (assert ∷ ASSERT, console ∷ CONSOLE, ref ∷ REF, exception ∷ EXCEPTION, avar ∷ AVAR, timer ∷ TIMER | eff)
type TestEff eff = Eff (TestEffects eff)
type TestAff eff = Aff (TestEffects eff)

newRef ∷ ∀ m eff a. MonadEff (ref ∷ REF | eff) m ⇒ a → m (Ref a)
newRef = liftEff <<< Ref.newRef

readRef ∷ ∀ m eff a. MonadEff (ref ∷ REF | eff) m ⇒ Ref a → m a
readRef = liftEff <<< Ref.readRef

writeRef ∷ ∀ m eff a. MonadEff (ref ∷ REF | eff) m ⇒ Ref a → a → m Unit
writeRef r = liftEff <<< Ref.writeRef r

modifyRef ∷ ∀ m eff a. MonadEff (ref ∷ REF | eff) m ⇒ Ref a → (a → a) → m Unit
modifyRef r = liftEff <<< Ref.modifyRef r

assertEff ∷ ∀ eff. String → Either Error Boolean → Eff (TestEffects eff) Unit
assertEff s = case _ of
  Left err → do
    Console.log ("[Error] " <> s)
    throwException err
  Right r → do
    assert' ("Assertion failure " <> s) r
    Console.log ("[OK] " <> s)

runAssert ∷ ∀ eff. String → TestAff eff Boolean → TestEff eff Unit
runAssert s = runAff_ (assertEff s)

runAssertEq ∷ ∀ eff a. Eq a ⇒ String → a → TestAff eff a → TestEff eff Unit
runAssertEq s a = runAff_ (assertEff s <<< map (eq a))

assertEq ∷ ∀ eff a. Eq a ⇒ String → a → TestAff eff a → TestAff eff Unit
assertEq s a aff = liftEff <<< assertEff s <<< map (eq a) =<< try aff

assert ∷ ∀ eff. String → TestAff eff Boolean → TestAff eff Unit
assert s aff = liftEff <<< assertEff s =<< try aff

withTimeout ∷ ∀ eff a. Milliseconds → TestAff eff a → TestAff eff a
withTimeout ms aff =
  either throwError pure =<< sequential do
    parallel (try aff) <|> parallel (delay ms $> Left (error "Timed out"))

test_pure ∷ ∀ eff. TestEff eff Unit
test_pure = runAssertEq "pure" 42 (pure 42)

test_bind ∷ ∀ eff. TestEff eff Unit
test_bind = runAssertEq "bind" 44 do
  n1 ← pure 42
  n2 ← pure (n1 + 1)
  n3 ← pure (n2 + 1)
  pure n3

test_try ∷ ∀ eff. TestEff eff Unit
test_try = runAssert "try" do
  n ← try (pure 42)
  case n of
    Right 42 → pure true
    _ → pure false

test_throw ∷ ∀ eff. TestEff eff Unit
test_throw = runAssert "try/throw" do
  n ← try (throwError (error "Nope."))
  pure (isLeft n)

test_liftEff ∷ ∀ eff. TestEff eff Unit
test_liftEff = runAssertEq "liftEff" 42 do
  ref ← newRef 0
  liftEff do
    writeRef ref 42
    readRef ref

test_delay ∷ ∀ eff. TestAff eff Unit
test_delay = assert "delay" do
  delay (Milliseconds 1000.0)
  pure true

test_fork ∷ ∀ eff. TestAff eff Unit
test_fork = assert "fork" do
  ref ← newRef ""
  fiber ← forkAff do
    delay (Milliseconds 10.0)
    modifyRef ref (_ <> "child")
  modifyRef ref (_ <> "go")
  delay (Milliseconds 20.0)
  modifyRef ref (_ <> "parent")
  eq "gochildparent" <$> readRef ref

test_join ∷ ∀ eff. TestAff eff Unit
test_join = assert "join" do
  ref ← newRef ""
  fiber ← forkAff do
    delay (Milliseconds 10.0)
    modifyRef ref (_ <> "child")
    readRef ref
  modifyRef ref (_ <> "parent")
  eq "parentchild" <$> joinFiber fiber

test_join_throw ∷ ∀ eff. TestAff eff Unit
test_join_throw = assert "join/throw" do
  fiber ← forkAff do
    delay (Milliseconds 10.0)
    throwError (error "Nope.")
  isLeft <$> try (joinFiber fiber)

test_join_throw_sync ∷ ∀ eff. TestAff eff Unit
test_join_throw_sync = assert "join/throw/sync" do
  fiber ← forkAff (throwError (error "Nope."))
  isLeft <$> try (joinFiber fiber)

test_multi_join ∷ ∀ eff. TestAff eff Unit
test_multi_join = assert "join/multi" do
  ref ← newRef 1
  f1 ← forkAff do
    delay (Milliseconds 10.0)
    modifyRef ref (_ + 1)
    pure 10
  f2 ← forkAff do
    delay (Milliseconds 20.0)
    modifyRef ref (_ + 1)
    pure 20
  n1 ← traverse joinFiber
    [ f1
    , f1
    , f1
    , f2
    ]
  n2 ← readRef ref
  pure (sum n1 == 50 && n2 == 3)

test_suspend ∷ ∀ eff. TestAff eff Unit
test_suspend = assert "suspend" do
  ref ← newRef ""
  fiber ← suspendAff do
    delay (Milliseconds 10.0)
    modifyRef ref (_ <> "child")
  modifyRef ref (_ <> "go")
  delay (Milliseconds 20.0)
  modifyRef ref (_ <> "parent")
  _ ← joinFiber fiber
  eq "goparentchild" <$> readRef ref

test_makeAff ∷ ∀ eff. TestAff eff Unit
test_makeAff = assert "makeAff" do
  ref1 ← newRef Nothing
  ref2 ← newRef 0
  fiber ← forkAff do
    n ← makeAff \cb → do
      writeRef ref1 (Just cb)
      pure mempty
    writeRef ref2 n
  delay (Milliseconds 5.0)
  cb ← readRef ref1
  case cb of
    Just k → do
      liftEff $ k (Right 42)
      _ ← joinFiber fiber
      eq 42 <$> readRef ref2
    Nothing → pure false

test_bracket ∷ ∀ eff. TestAff eff Unit
test_bracket = assert "bracket" do
  ref ← newRef []
  let
    action s = do
      delay (Milliseconds 10.0)
      modifyRef ref (_ <> [ s ])
      pure s
  fiber ← forkAff do
    delay (Milliseconds 40.0)
    readRef ref
  _ ← bracket
    (action "foo")
    (\s → void $ action (s <> "/release"))
    (\s → action (s <> "/run"))
  joinFiber fiber <#> eq
    [ "foo"
    , "foo/run"
    , "foo/release"
    ]

test_bracket_nested ∷ ∀ eff. TestAff eff Unit
test_bracket_nested = assert "bracket/nested" do
  ref ← newRef []
  let
    action s = do
      delay (Milliseconds 10.0)
      modifyRef ref (_ <> [ s ])
      pure s
    bracketAction s =
      bracket
        (action (s <> "/bar"))
        (\s' → void $ action (s' <> "/release"))
        (\s' → action (s' <> "/run"))
  _ ← bracket
    (bracketAction "foo")
    (\s → void $ bracketAction (s <> "/release"))
    (\s → bracketAction (s <> "/run"))
  readRef ref <#> eq
    [ "foo/bar"
    , "foo/bar/run"
    , "foo/bar/release"
    , "foo/bar/run/run/bar"
    , "foo/bar/run/run/bar/run"
    , "foo/bar/run/run/bar/release"
    , "foo/bar/run/release/bar"
    , "foo/bar/run/release/bar/run"
    , "foo/bar/run/release/bar/release"
    ]

test_general_bracket ∷ ∀ eff. TestAff eff Unit
test_general_bracket = assert "bracket/general" do
  ref ← newRef ""
  let
    action s = do
      delay (Milliseconds 10.0)
      modifyRef ref (_ <> s)
      pure s
    bracketAction s =
      generalBracket (action s)
        { killed: \error s' → void $ action (s' <> "/kill/" <> message error)
        , failed: \error s' → void $ action (s' <> "/throw/" <> message error)
        , completed: \r s' → void $ action (s' <> "/release/" <> r)
        }

  f1 ← forkAff $ bracketAction "foo" (const (action "a"))
  delay (Milliseconds 5.0)
  killFiber (error "z") f1
  r1 ← try $ joinFiber f1

  f2 ← forkAff $ bracketAction "bar" (const (throwError $ error "b"))
  r2 ← try $ joinFiber f2

  f3 ← forkAff $ bracketAction "baz" (const (action "c"))
  r3 ← try $ joinFiber f3

  r4 ← readRef ref
  pure (isLeft r1 && isLeft r2 && isRight r3 && r4 == "foofoo/kill/zbarbar/throw/bbazcbaz/release/c")

test_supervise ∷ ∀ eff. TestAff eff Unit
test_supervise = assert "supervise" do
  ref ← newRef ""
  r1 ← supervise do
    _ ← forkAff do
      bracket
        (modifyRef ref (_ <> "acquire"))
        (\_ → modifyRef ref (_ <> "release"))
        (\_ → delay (Milliseconds 10.0))
    _ ← forkAff do
      delay (Milliseconds 11.0)
      modifyRef ref (_ <> "delay")
    delay (Milliseconds 5.0)
    modifyRef ref (_ <> "done")
    pure "done"
  delay (Milliseconds 20.0)
  r2 ← readRef ref
  pure (r1 == "done" && r2 == "acquiredonerelease")

test_kill ∷ ∀ eff. TestAff eff Unit
test_kill = assert "kill" do
  fiber ← forkAff never
  killFiber (error "Nope") fiber
  isLeft <$> try (joinFiber fiber)

test_kill_canceler ∷ ∀ eff. TestAff eff Unit
test_kill_canceler = assert "kill/canceler" do
  ref ← newRef ""
  fiber ← forkAff do
    n ← makeAff \_ → pure $ Canceler \_ → do
      delay (Milliseconds 20.0)
      liftEff (writeRef ref "cancel")
    writeRef ref "done"
  delay (Milliseconds 10.0)
  killFiber (error "Nope") fiber
  res ← try (joinFiber fiber)
  n ← readRef ref
  pure (n == "cancel" && (lmap message res) == Left "Nope")

test_kill_bracket ∷ ∀ eff. TestAff eff Unit
test_kill_bracket = assert "kill/bracket" do
  ref ← newRef ""
  let
    action n = do
      delay (Milliseconds 10.0)
      modifyRef ref (_ <> n)
  fiber ←
    forkAff $ bracket
      (action "a")
      (\_ → action "b")
      (\_ → action "c")
  delay (Milliseconds 5.0)
  killFiber (error "Nope") fiber
  _ ← try (joinFiber fiber)
  eq "ab" <$> readRef ref

test_kill_bracket_nested ∷ ∀ eff. TestAff eff Unit
test_kill_bracket_nested = assert "kill/bracket/nested" do
  ref ← newRef []
  let
    action s = do
      delay (Milliseconds 10.0)
      modifyRef ref (_ <> [ s ])
      pure s
    bracketAction s =
      bracket
        (action (s <> "/bar"))
        (\s' → void $ action (s' <> "/release"))
        (\s' → action (s' <> "/run"))
  fiber ←
    forkAff $ bracket
      (bracketAction "foo")
      (\s → void $ bracketAction (s <> "/release"))
      (\s → bracketAction (s <> "/run"))
  delay (Milliseconds 5.0)
  killFiber (error "Nope") fiber
  _ ← try (joinFiber fiber)
  readRef ref <#> eq
    [ "foo/bar"
    , "foo/bar/run"
    , "foo/bar/release"
    , "foo/bar/run/release/bar"
    , "foo/bar/run/release/bar/run"
    , "foo/bar/run/release/bar/release"
    ]

test_kill_supervise ∷ ∀ eff. TestAff eff Unit
test_kill_supervise = assert "kill/supervise" do
  ref ← newRef ""
  let
    action s = generalBracket
      (modifyRef ref (_ <> "acquire" <> s))
      { failed: \_ _ → modifyRef ref (_ <> "throw" <> s)
      , killed: \_ _ → modifyRef ref (_ <> "kill" <> s)
      , completed: \_ _ → modifyRef ref (_ <> "complete" <> s)
      }
      (\_ -> do
        delay (Milliseconds 10.0)
        modifyRef ref (_ <> "child" <> s))
  fiber ← forkAff $ supervise do
    _ ← forkAff $ action "foo"
    _ ← forkAff $ action "bar"
    delay (Milliseconds 5.0)
    modifyRef ref (_ <> "parent")
  delay (Milliseconds 1.0)
  killFiber (error "nope") fiber
  delay (Milliseconds 20.0)
  eq "acquirefooacquirebarkillfookillbar" <$> readRef ref

test_kill_finalizer_catch ∷ ∀ eff. TestAff eff Unit
test_kill_finalizer_catch = assert "kill/finalizer/catch" do
  ref ← newRef ""
  fiber ← forkAff $ bracket
    (delay (Milliseconds 10.0))
    (\_ → throwError (error "Finalizer") `catchError` \_ → writeRef ref "caught")
    (\_ → pure unit)
  killFiber (error "Nope") fiber
  eq "caught" <$> readRef ref

test_kill_finalizer_bracket ∷ ∀ eff. TestAff eff Unit
test_kill_finalizer_bracket = assert "kill/finalizer/bracket" do
  ref ← newRef ""
  fiber ← forkAff $ bracket
    (delay (Milliseconds 10.0))
    (\_ → generalBracket (pure unit)
      { killed: \_ _ → writeRef ref "killed"
      , failed: \_ _ → writeRef ref "failed"
      , completed: \_ _ → writeRef ref "completed"
      }
      (\_ → pure unit))
    (\_ → pure unit)
  killFiber (error "Nope") fiber
  eq "completed" <$> readRef ref

test_parallel ∷ ∀ eff. TestAff eff Unit
test_parallel = assert "parallel" do
  ref ← newRef ""
  let
    action s = do
      delay (Milliseconds 10.0)
      modifyRef ref (_ <> s)
      pure s
  f1 ← forkAff $ sequential $
    { a: _, b: _ }
      <$> parallel (action "foo")
      <*> parallel (action "bar")
  delay (Milliseconds 15.0)
  r1 ← readRef ref
  r2 ← joinFiber f1
  pure (r1 == "foobar" && r2.a == "foo" && r2.b == "bar")

test_parallel_throw ∷ ∀ eff. TestAff eff Unit
test_parallel_throw = assert "parallel/throw" $ withTimeout (Milliseconds 100.0) do
  ref ← newRef ""
  let
    action n s = do
      delay (Milliseconds n)
      modifyRef ref (_ <> s)
      pure s
  r1 ← try $ sequential $
    { a: _, b: _ }
      <$> parallel (action 10.0 "foo" *> throwError (error "Nope"))
      <*> parallel never
  r2 ← readRef ref
  pure (isLeft r1 && r2 == "foo")

test_kill_parallel ∷ ∀ eff. TestAff eff Unit
test_kill_parallel = assert "kill/parallel" do
  ref ← newRef ""
  let
    action s = do
      bracket
        (pure unit)
        (\_ → modifyRef ref (_ <> "killed" <> s))
        (\_ → do
          delay (Milliseconds 10.0)
          modifyRef ref (_ <> s))
  f1 ← forkAff $ sequential $
    parallel (action "foo") *> parallel (action "bar")
  f2 ← forkAff do
    delay (Milliseconds 5.0)
    killFiber (error "Nope") f1
    modifyRef ref (_ <> "done")
  _ ← try $ joinFiber f1
  _ ← try $ joinFiber f2
  eq "killedfookilledbardone" <$> readRef ref

test_parallel_alt ∷ ∀ eff. TestAff eff Unit
test_parallel_alt = assert "parallel/alt" do
  ref ← newRef ""
  let
    action n s = do
      delay (Milliseconds n)
      modifyRef ref (_ <> s)
      pure s
  f1 ← forkAff $ sequential $
    parallel (action 10.0 "foo") <|> parallel (action 5.0 "bar")
  delay (Milliseconds 10.0)
  r1 ← readRef ref
  r2 ← joinFiber f1
  pure (r1 == "bar" && r2 == "bar")

test_parallel_alt_throw ∷ ∀ eff. TestAff eff Unit
test_parallel_alt_throw = assert "parallel/alt/throw" do
  r1 ← sequential $
    parallel (delay (Milliseconds 10.0) *> throwError (error "Nope."))
    <|> parallel (delay (Milliseconds 11.0) $> "foo")
    <|> parallel (delay (Milliseconds 12.0) $> "bar")
  pure (r1 == "foo")

test_parallel_alt_sync ∷ ∀ eff. TestAff eff Unit
test_parallel_alt_sync = assert "parallel/alt/sync" do
  ref ← newRef ""
  let
    action s = do
      bracket
        (pure unit)
        (\_ → modifyRef ref (_ <> "killed" <> s))
        (\_ → modifyRef ref (_ <> s) $> s)
  r1 ← sequential $
    parallel (action "foo")
    <|> parallel (action "bar")
    <|> parallel (action "baz")
  r2 ← readRef ref
  pure (r1 == "foo" && r2 == "fookilledfoo")

test_parallel_mixed ∷ ∀ eff. TestAff eff Unit
test_parallel_mixed = assert "parallel/mixed" do
  ref ← newRef ""
  let
    action n s = parallel do
      delay (Milliseconds n)
      modifyRef ref (_ <> s)
      pure s
  { r1, r2, r3 } ← sequential $
    { r1: _, r2: _, r3: _ }
      <$> action 10.0 "a"
      <*> (action 15.0 "a"
            <|> action 12.0 "b"
            <|> action 16.0 "c")
      <*> (action 15.0 "a"
            <|> ((<>) <$> action 13.0 "d" <*> action 14.0 "e")
            <|> action 16.0 "f")
  delay (Milliseconds 20.0)
  r4 ← readRef ref
  pure (r1 == "a" && r2 == "b" && r3 == "de" && r4 == "abde")

test_kill_parallel_alt ∷ ∀ eff. TestAff eff Unit
test_kill_parallel_alt = assert "kill/parallel/alt" do
  ref ← newRef ""
  let
    action n s = do
      bracket
        (pure unit)
        (\_ → modifyRef ref (_ <> "killed" <> s))
        (\_ → do
          delay (Milliseconds n)
          modifyRef ref (_ <> s))
  f1 ← forkAff $ sequential $
    parallel (action 10.0 "foo") <|> parallel (action 20.0 "bar")
  f2 ← forkAff do
    delay (Milliseconds 5.0)
    killFiber (error "Nope") f1
    modifyRef ref (_ <> "done")
  _ ← try $ joinFiber f1
  _ ← try $ joinFiber f2
  eq "killedfookilledbardone" <$> readRef ref

test_kill_parallel_alt_finalizer ∷ ∀ eff. TestAff eff Unit
test_kill_parallel_alt_finalizer = assert "kill/parallel/alt/finalizer" do
  ref ← newRef ""
  f1 ← forkAff $ sequential $
    parallel (delay (Milliseconds 10.0)) <|> parallel do
      bracket
        (pure unit)
        (\_ → do
          delay (Milliseconds 10.0)
          modifyRef ref (_ <> "killed"))
        (\_ → delay (Milliseconds 20.0))
  f2 ← forkAff do
    delay (Milliseconds 15.0)
    killFiber (error "Nope") f1
    modifyRef ref (_ <> "done")
  _ ← try $ joinFiber f1
  _ ← try $ joinFiber f2
  eq "killeddone" <$> readRef ref

test_fiber_map ∷ ∀ eff. TestAff eff Unit
test_fiber_map = assert "fiber/map" do
  ref ← newRef 0
  let
    mapFn a = runPure do
      unsafeRunRef $ Ref.modifyRef ref (_ + 1)
      pure (a + 1)
  f1 ← forkAff do
    delay (Milliseconds 10.0)
    pure 10
  let
    f2 = mapFn <$> f1
  a ← joinFiber f2
  b ← joinFiber f2
  n ← readRef ref
  pure (a == 11 && b == 11 && n == 1)

test_fiber_apply ∷ ∀ eff. TestAff eff Unit
test_fiber_apply = assert "fiber/apply" do
  ref ← newRef 0
  let
    applyFn a b = runPure do
      unsafeRunRef $ Ref.modifyRef ref (_ + 1)
      pure (a + b)
  f1 ← forkAff do
    delay (Milliseconds 10.0)
    pure 10
  f2 ← forkAff do
    delay (Milliseconds 15.0)
    pure 12
  let
    f3 = applyFn <$> f1 <*> f2
  a ← joinFiber f3
  b ← joinFiber f3
  n ← readRef ref
  pure (a == 22 && b == 22 && n == 1)

test_avar_order ∷ ∀ eff. TestAff eff Unit
test_avar_order = assert "avar/order" do
  ref ← newRef ""
  var ← makeEmptyVar
  f1 ← forkAff do
    delay (Milliseconds 10.0)
    value ← takeVar var
    modifyRef ref (_ <> value)
  putVar "foo" var
  modifyRef ref (_ <> "taken")
  joinFiber f1
  eq "takenfoo" <$> readRef ref

test_efffn ∷ ∀ eff. TestAff eff Unit
test_efffn = assert "efffn" do
  ref ← newRef ""
  let
    jsDelay ms = AC.fromEffFnAff $ AC.EffFnAff $ AC.mkEffFn2 \ke kc → do
      tid ← setTimeout ms (AC.runEffFn1 kc unit)
      pure $ AC.EffFnCanceler $ AC.mkEffFn3 \e cke ckc → do
        clearTimeout tid
        AC.runEffFn1 ckc unit
    action = do
      jsDelay 10
      modifyRef ref (_ <> "done")
  f1 ← forkAff action
  f2 ← forkAff action
  killFiber (error "Nope.") f2
  delay (Milliseconds 20.0)
  eq "done" <$> readRef ref

test_parallel_stack ∷ ∀ eff. TestAff eff Unit
test_parallel_stack = assert "parallel/stack" do
  ref ← newRef 0
  parTraverse_ (modifyRef ref <<< add) (Array.replicate 100000 1)
  eq 100000 <$> readRef ref

test_scheduler_size ∷ ∀ eff. TestAff eff Unit
test_scheduler_size = assert "scheduler" do
  ref ← newRef 0
  _ ← traverse joinFiber =<< traverse forkAff (Array.replicate 100000 (modifyRef ref (add 1)))
  eq 100000 <$> readRef ref

test_lazy ∷ ∀ eff. TestAff eff Unit
test_lazy = assert "lazy" do
  varA ← makeEmptyVar
  varB ← makeEmptyVar
  fiberA <- forkAff $ fix \loop -> do
    a <- takeVar varA
    putVar (a + 1) varB
    loop
  fiberB <- forkAff $ fix \loop -> do
    b <- takeVar varB
    if (b > 100)
      then do
        killFiber (error "finished") fiberA
        pure "done"
      else do
        putVar (b + 1) varA
        loop
  putVar 0 varA
  eq "done" <$> joinFiber fiberB

test_regression_return_fork ∷ ∀ eff. TestAff eff Unit
test_regression_return_fork = assert "regression/return-fork" do
  bracket
    (forkAff (pure unit))
    (const (pure unit))
    (const (pure true))

main ∷ TestEff () Unit
main = do
  test_pure
  test_bind
  test_try
  test_throw
  test_liftEff

  void $ launchAff do
    test_delay
    test_fork
    test_join
    test_join_throw
    test_join_throw_sync
    test_multi_join
    test_suspend
    test_makeAff
    test_bracket
    test_bracket_nested
    test_general_bracket
    test_supervise
    test_kill
    test_kill_canceler
    test_kill_bracket
    test_kill_bracket_nested
    test_kill_supervise
    test_kill_finalizer_catch
    test_kill_finalizer_bracket
    test_parallel
    test_parallel_throw
    test_kill_parallel
    test_parallel_alt
    test_parallel_alt_throw
    test_parallel_alt_sync
    test_parallel_mixed
    test_kill_parallel_alt
    test_kill_parallel_alt_finalizer
    test_avar_order
    test_lazy
    test_efffn
    test_fiber_map
    test_fiber_apply
    -- Turn on if we decide to schedule forks
    -- test_scheduler_size
    test_parallel_stack
    test_regression_return_fork
