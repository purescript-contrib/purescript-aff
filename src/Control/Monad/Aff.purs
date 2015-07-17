module Control.Monad.Aff
  ( Aff()
  , Canceler(..)
  , PureAff(..)
  , apathize
  , attempt
  , cancel
  , cancelWith
  , finally
  , forkAff
  , later
  , later'
  , launchAff
  , liftEff'
  , makeAff
  , makeAff'
  , nonCanceler
  , runAff
  )
  where

  import Prelude

  import Control.Alt(Alt)
  import Control.Alternative(Alternative)
  import Control.Monad.Cont.Class(MonadCont)
  import Control.Monad.Eff(Eff())
  import Control.Monad.Eff.Class(MonadEff, liftEff)
  import Control.Monad.Eff.Exception(Error(), EXCEPTION(), catchException, error)
  import Control.Monad.Eff.Unsafe(unsafeInterleaveEff)
  import Control.Monad.Error.Class(MonadError, throwError)
  import Control.Monad.Rec.Class(MonadRec, tailRecM)
  import Control.MonadPlus(MonadPlus)
  import Control.Plus(Plus)

  import Data.Either(Either(..), either)
  import Data.Function(Fn2(), Fn3(), runFn2, runFn3)
  import Data.Monoid(Monoid, mempty)

  -- | An asynchronous computation with effects `e`. The computation either
  -- | errors or produces a value of type `a`.
  -- |
  -- | This is moral equivalent of `ErrorT (ContT Unit (Eff e)) a`.
  foreign import data Aff :: # ! -> * -> *

  -- | A pure asynchronous computation, having no effects other than
  -- | asynchronous computation.
  type PureAff a = forall e. Aff e a

  -- | A canceler is asynchronous function that can be used to attempt the
  -- | cancelation of a computation. Returns a boolean flag indicating whether
  -- | or not the cancellation was successful. Many computations may be composite,
  -- | in such cases the flag indicates whether any part of the computation was
  -- | successfully canceled. The flag should not be used for communication.
  newtype Canceler e = Canceler (Error -> Aff e Boolean)

  -- | Unwraps the canceler function from the newtype that wraps it.
  cancel :: forall e. Canceler e -> Error ->  Aff e Boolean
  cancel (Canceler f) = f

  -- | This function allows you to attach a custom canceler to an asynchronous
  -- | computation. If the computation is canceled, then the custom canceler
  -- | will be run along side the computation's own canceler.
  cancelWith :: forall e a. Aff e a -> Canceler e -> Aff e a
  cancelWith aff c = runFn3 _cancelWith nonCanceler aff c

  -- | Converts the asynchronous computation into a synchronous one. All values
  -- | and errors are ignored.
  launchAff :: forall e a. Aff e a -> Eff e Unit
  launchAff = runAff (const (pure unit)) (const (pure unit))

  -- | Runs the asynchronous computation. You must supply an error callback and a
  -- | success callback.
  runAff :: forall e a. (Error -> Eff e Unit) -> (a -> Eff e Unit) -> Aff e a -> Eff e Unit
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

  instance monadPlusAff :: MonadPlus (Aff e)

  instance monadRecAff :: MonadRec (Aff e) where
    tailRecM f a = go 0 f a
      where
      go size f a = do
        e <- f a
        case e of
          Left a' | size < 100 -> go (size + 1) f a'
                  | otherwise -> later (tailRecM f a')
          Right b -> pure b

  instance monadContAff :: MonadCont (Aff e) where
    callCC f = makeAff (\eb cb -> runAff eb cb (f \a -> makeAff (\_ _ -> cb a)))

  instance semigroupCanceler :: Semigroup (Canceler e) where
    append (Canceler f1) (Canceler f2) = Canceler (\e -> (||) <$> f1 e <*> f2 e)

  instance monoidCanceler :: Monoid (Canceler e) where
    mempty = Canceler (const (pure true))

  foreign import _cancelWith :: forall e a. Fn3 (Canceler e) (Aff e a) (Canceler e) (Aff e a)

  foreign import _setTimeout :: forall e a. Fn3 (Canceler e) Int (Aff e a) (Aff e a)

  foreign import _unsafeInterleaveAff :: forall e1 e2 a. Aff e1 a -> Aff e2 a

  foreign import _forkAff :: forall e a. Fn2 (Canceler e) (Aff e a) (Aff e (Canceler e))

  foreign import _makeAff :: forall e a. ((Error -> Eff e Unit) -> (a -> Eff e Unit) -> Eff e (Canceler e)) -> Aff e a

  foreign import _pure :: forall e a. Fn2 (Canceler e) a (Aff e a)

  foreign import _throwError :: forall e a. Fn2 (Canceler e) Error (Aff e a)

  foreign import _fmap :: forall e a b. Fn2 (a -> b) (Aff e a) (Aff e b)

  foreign import _bind :: forall e a b. Fn3 (Canceler e) (Aff e a) (a -> Aff e b) (Aff e b)

  foreign import _attempt :: forall e a. Fn3 (forall x y. x -> Either x y) (forall x y. y -> Either x y) (Aff e a) (Aff e (Either Error a))

  foreign import _runAff :: forall e a. Fn3 (Error -> Eff e Unit) (a -> Eff e Unit) (Aff e a) (Eff e Unit)

  foreign import _liftEff :: forall e a. Fn2 (Canceler e) (Eff e a) (Aff e a)



