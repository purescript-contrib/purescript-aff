module Control.Monad.Aff
  ( Aff
  , Canceler(..)
  , PureAff(..)
  , apathize
  , attempt
  , cancel
  , cancelWith
  , finally
  , forkAff
  , forkAll
  , later
  , later'
  , launchAff
  , liftEff'
  , makeAff
  , makeAff'
  , nonCanceler
  , runAff
  , ParAff(..)
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Aff.Internal (AVBox, AVar, _killVar, _putVar, _takeVar, _makeVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (Error, EXCEPTION, throwException, error)
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Control.MonadPlus (class MonadZero, class MonadPlus)
import Control.Parallel (class Parallel)
import Control.Plus (class Plus, empty)

import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, foldl)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), fst, snd)

import Unsafe.Coerce (unsafeCoerce)

-- | An asynchronous computation with effects `e`. The computation either
-- | errors or produces a value of type `a`.
-- |
-- | This is moral equivalent of `ErrorT (ContT Unit (Eff e)) a`.
foreign import data Aff :: # ! -> * -> *

-- | A pure asynchronous computation, having no effects other than
-- | asynchronous computation.
type PureAff a = forall e. Aff e a

-- | A canceler is an asynchronous function that can be used to attempt the
-- | cancelation of a computation. Returns a boolean flag indicating whether
-- | or not the cancellation was successful. Many computations may be composite,
-- | in such cases the flag indicates whether any part of the computation was
-- | successfully canceled. The flag should not be used for communication.
newtype Canceler e = Canceler (Error -> Aff e Boolean)

-- | Unwraps the canceler function from the newtype that wraps it.
cancel :: forall e. Canceler e -> Error -> Aff e Boolean
cancel (Canceler f) = f

-- | This function allows you to attach a custom canceler to an asynchronous
-- | computation. If the computation is canceled, then the custom canceler
-- | will be run along side the computation's own canceler.
cancelWith :: forall e a. Aff e a -> Canceler e -> Aff e a
cancelWith aff c = runFn3 _cancelWith nonCanceler aff c

-- | Converts the asynchronous computation into a synchronous one. All values
-- | are ignored, and if the computation produces an error, it is thrown.
-- |
-- | Catching exceptions by using `catchException` with the resulting Eff
-- | computation is not recommended, as exceptions may end up being thrown
-- | asynchronously, in which case they cannot be caught.
-- |
-- | If you do need to handle exceptions, you can use `runAff` instead, or
-- | you can handle the exception within the Aff computation, using
-- | `catchError` (or any of the other mechanisms).
launchAff :: forall e a. Aff e a -> Eff (err :: EXCEPTION | e) (Canceler e)
launchAff = lowerEx <<< runAff throwException (const (pure unit)) <<< liftEx
  where
  liftEx :: Aff e a -> Aff (err :: EXCEPTION | e) a
  liftEx = _unsafeInterleaveAff
  lowerEx :: Eff (err :: EXCEPTION | e) (Canceler (err :: EXCEPTION | e)) -> Eff (err :: EXCEPTION | e) (Canceler e)
  lowerEx = map (Canceler <<< map _unsafeInterleaveAff <<< cancel)

-- | Runs the asynchronous computation. You must supply an error callback and a
-- | success callback.
-- |
-- | Returns a canceler that can be used to attempt cancellation of the
-- | asynchronous computation.
runAff :: forall e a. (Error -> Eff e Unit) -> (a -> Eff e Unit) -> Aff e a -> Eff e (Canceler e)
runAff ex f aff = runFn3 _runAff ex f aff

-- | Creates an asynchronous effect from a function that accepts error and
-- | success callbacks. This function can be used for asynchronous computations
-- | that cannot be canceled.
makeAff :: forall e a. ((Error -> Eff e Unit) -> (a -> Eff e Unit) -> Eff e Unit) -> Aff e a
makeAff h = makeAff' (\e a -> const nonCanceler <$> h e a)

-- | Creates an asynchronous effect from a function that accepts error and
-- | success callbacks, and returns a canceler for the computation. This
-- | function can be used for asynchronous computations that can be canceled.
makeAff' :: forall e a. ((Error -> Eff e Unit) -> (a -> Eff e Unit) -> Eff e (Canceler e)) -> Aff e a
makeAff' h = _makeAff h

-- | Runs the asynchronous computation off the current execution context.
later :: forall e a. Aff e a -> Aff e a
later = later' 0

-- | Runs the specified asynchronous computation later, by the specified
-- | number of milliseconds.
later' :: forall e a. Int -> Aff e a -> Aff e a
later' n aff = runFn3 _setTimeout nonCanceler n aff

-- | Compute `aff1`, followed by `aff2` regardless of whether `aff1` terminated successfully.
finally :: forall e a b. Aff e a -> Aff e b -> Aff e a
finally aff1 aff2 = do
  x <- attempt aff1
  aff2
  either throwError pure x

-- | Forks the specified asynchronous computation so subsequent computations
-- | will not block on the result of the computation.
-- |
-- | Returns a canceler that can be used to attempt cancellation of the
-- | forked computation.
forkAff :: forall e a. Aff e a -> Aff e (Canceler e)
forkAff aff = runFn2 _forkAff nonCanceler aff

-- | Forks many asynchronous computation in a synchronous manner while being
-- | stack-safe up to the selected Foldable instance.
-- |
-- | Returns a canceler that can be used to attempt cancellation of all
-- | forked computations.
forkAll :: forall f e a. (Foldable f) => f (Aff e a) -> Aff e (Canceler e)
forkAll affs = runFn3 _forkAll nonCanceler foldl affs

-- | Promotes any error to the value level of the asynchronous monad.
attempt :: forall e a. Aff e a -> Aff e (Either Error a)
attempt aff = runFn3 _attempt Left Right aff

-- | Ignores any errors.
apathize :: forall e a. Aff e a -> Aff e Unit
apathize a = const unit <$> attempt a

-- | Lifts a synchronous computation and makes explicit any failure from exceptions.
liftEff' :: forall e a. Eff (err :: EXCEPTION | e) a -> Aff e (Either Error a)
liftEff' eff = attempt (_unsafeInterleaveAff (runFn2 _liftEff nonCanceler eff))

-- | A constant canceller that always returns false.
nonCanceler :: forall e. Canceler e
nonCanceler = Canceler (const (pure false))

-- | A constant canceller that always returns true.
alwaysCanceler :: forall e. Canceler e
alwaysCanceler = Canceler (const (pure true))

instance semigroupAff :: (Semigroup a) => Semigroup (Aff e a) where
  append a b = (<>) <$> a <*> b

instance monoidAff :: (Monoid a) => Monoid (Aff e a) where
  mempty = pure mempty

instance functorAff :: Functor (Aff e) where
  map f fa = runFn2 _fmap f fa

instance applyAff :: Apply (Aff e) where
  apply ff fa = runFn3 _bind alwaysCanceler ff (\f -> f <$> fa)

instance applicativeAff :: Applicative (Aff e) where
  pure v = runFn2 _pure nonCanceler v

instance bindAff :: Bind (Aff e) where
  bind fa f = runFn3 _bind alwaysCanceler fa f

instance monadAff :: Monad (Aff e)

instance monadEffAff :: MonadEff e (Aff e) where
  liftEff eff = runFn2 _liftEff nonCanceler eff

-- | Allows users to catch and throw errors on the error channel of the
-- | asynchronous computation. See documentation in `purescript-transformers`.
instance monadErrorAff :: MonadError Error (Aff e) where
  throwError e = runFn2 _throwError nonCanceler e

  catchError aff ex = attempt aff >>= either ex pure

instance altAff :: Alt (Aff e) where
  alt a1 a2 = attempt a1 >>= either (const a2) pure

instance plusAff :: Plus (Aff e) where
  empty = throwError $ error "Always fails"

instance alternativeAff :: Alternative (Aff e)

instance monadZero :: MonadZero (Aff e)

instance monadPlusAff :: MonadPlus (Aff e)

instance monadRecAff :: MonadRec (Aff e) where
  tailRecM f a = runFn3 _tailRecM isLoop f a
    where
    isLoop (Loop _) = true
    isLoop _ = false

instance semigroupCanceler :: Semigroup (Canceler e) where
  append (Canceler f1) (Canceler f2) = Canceler (\e -> (||) <$> f1 e <*> f2 e)

instance monoidCanceler :: Monoid (Canceler e) where
  mempty = Canceler (const (pure true))

newtype ParAff e a = ParAff (Aff e a)

derive instance newtypeParAff :: Newtype (ParAff e a) _

instance semigroupParAff :: (Semigroup a) => Semigroup (ParAff e a) where
  append a b = append <$> a <*> b

instance monoidParAff :: (Monoid a) => Monoid (ParAff e a) where
  mempty = pure mempty

derive newtype instance functorParAff :: Functor (ParAff e)

instance applyParAff :: Apply (ParAff e) where
  apply (ParAff ff) (ParAff fa) = ParAff do
    va <- makeVar
    vb <- makeVar
    c1 <- forkAff (putOrKill va =<< attempt ff)
    c2 <- forkAff (putOrKill vb =<< attempt fa)
    (takeVar va <*> takeVar vb) `cancelWith` (c1 <> c2)
    where
    putOrKill :: forall a. AVar a -> Either Error a -> Aff e Unit
    putOrKill v = either (killVar v) (putVar v)

instance applicativeParAff :: Applicative (ParAff e) where
  pure = ParAff <<< pure

-- | Returns the first value, or the first error if both error.
instance altParAff :: Alt (ParAff e) where
  alt (ParAff a1) (ParAff a2) = ParAff do
    va <- makeVar -- the `a` value
    ve <- makeVar -- the error count (starts at 0)
    cs <- makeVar -- the cancelers
    putVar ve 0
    c1 <- forkAff $ either (maybeKill va ve) (done cs snd va) =<< attempt a1
    c2 <- forkAff $ either (maybeKill va ve) (done cs fst va) =<< attempt a2
    putVar cs (Tuple c1 c2)
    takeVar va `cancelWith` (c1 <> c2)
    where
    done :: forall a. AVar (Tuple (Canceler e) (Canceler e)) -> (forall x. Tuple x x -> x) -> AVar a -> a -> Aff e Unit
    done cs get va x = do
      putVar va x
      c <- get <$> takeVar cs
      void $ cancel c (error "Alt early exit")
    maybeKill :: forall a. AVar a -> AVar Int -> Error -> Aff e Unit
    maybeKill va ve err = do
      e <- takeVar ve
      when (e == 1) $ killVar va err
      putVar ve (e + 1)

instance plusParAff :: Plus (ParAff e) where
  empty = ParAff empty

instance alternativeParAff :: Alternative (ParAff e)

instance parallelParAff :: Parallel (ParAff e) (Aff e) where
  parallel = ParAff
  sequential (ParAff ma) = ma

makeVar :: forall e a. Aff e (AVar a)
makeVar = fromAVBox $ _makeVar nonCanceler

takeVar :: forall e a. AVar a -> Aff e a
takeVar q = fromAVBox $ runFn2 _takeVar nonCanceler q

putVar :: forall e a. AVar a -> a -> Aff e Unit
putVar q a = fromAVBox $ runFn3 _putVar nonCanceler q a

killVar :: forall e a. AVar a -> Error -> Aff e Unit
killVar q e = fromAVBox $ runFn3 _killVar nonCanceler q e

fromAVBox :: forall a e. AVBox a -> Aff e a
fromAVBox = unsafeCoerce

foreign import _cancelWith :: forall e a. Fn3 (Canceler e) (Aff e a) (Canceler e) (Aff e a)

foreign import _setTimeout :: forall e a. Fn3 (Canceler e) Int (Aff e a) (Aff e a)

foreign import _unsafeInterleaveAff :: forall e1 e2 a. Aff e1 a -> Aff e2 a

foreign import _forkAff :: forall e a. Fn2 (Canceler e) (Aff e a) (Aff e (Canceler e))

foreign import _forkAll :: forall f e a b. Fn3 (Canceler e) ((b -> a -> b) -> b -> f a -> b) (f (Aff e a)) (Aff e (Canceler e))

foreign import _makeAff :: forall e a. ((Error -> Eff e Unit) -> (a -> Eff e Unit) -> Eff e (Canceler e)) -> Aff e a

foreign import _pure :: forall e a. Fn2 (Canceler e) a (Aff e a)

foreign import _throwError :: forall e a. Fn2 (Canceler e) Error (Aff e a)

foreign import _fmap :: forall e a b. Fn2 (a -> b) (Aff e a) (Aff e b)

foreign import _bind :: forall e a b. Fn3 (Canceler e) (Aff e a) (a -> Aff e b) (Aff e b)

foreign import _attempt :: forall e a. Fn3 (forall x y. x -> Either x y) (forall x y. y -> Either x y) (Aff e a) (Aff e (Either Error a))

foreign import _runAff :: forall e a. Fn3 (Error -> Eff e Unit) (a -> Eff e Unit) (Aff e a) (Eff e (Canceler e))

foreign import _liftEff :: forall e a. Fn2 (Canceler e) (Eff e a) (Aff e a)

foreign import _tailRecM :: forall e a b. Fn3 (Step a b -> Boolean) (a -> Aff e (Step a b)) a (Aff e b)
