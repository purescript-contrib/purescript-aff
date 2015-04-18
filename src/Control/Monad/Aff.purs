module Control.Monad.Aff
  ( Aff()
  , Canceler(..)
  , PureAff(..)
  , apathize
  , attempt
  , cancel
  , cancelWith
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

  import Data.Either(Either(..), either)
  import Data.Function(Fn2(), Fn3(), runFn2, runFn3)
  import Data.Monoid(Monoid, mempty)
  import Control.Apply
  import Control.Alt(Alt)
  import Control.Plus(Plus)
  import Control.Alternative(Alternative)
  import Control.MonadPlus(MonadPlus)
  import Control.Monad.Eff
  import Control.Monad.Eff.Exception(Error(), Exception(), catchException, error)
  import Control.Monad.Eff.Unsafe(unsafeInterleaveEff)
  import Control.Monad.Eff.Class
  import Control.Monad.Error.Class(MonadError, throwError)

  -- | A computation with effects `e`. The computation either errors or
  -- | produces a value of type `a`.
  -- |
  -- | This is moral equivalent of `ErrorT (ContT Unit (Eff e)) a`.
  foreign import data Aff :: # ! -> * -> *

  -- | A pure asynchronous computation, having no effects.
  type PureAff a = forall e. Aff e a

  -- | A canceler is asynchronous function that can be used to attempt the 
  -- | cancelation of a computation. Returns a boolean flag indicating whether
  -- | or not the cancellation was successful.
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

  -- | Runs the asynchronous computation later (off the current execution context).
  later' :: forall e a. Number -> Aff e a -> Aff e a
  later' n aff = runFn3 _setTimeout nonCanceler n aff

  -- | Forks the specified asynchronous computation so subsequent monadic binds
  -- | will not block on the result of the computation.
  forkAff :: forall e a. Aff e a -> Aff e (Canceler e)
  forkAff aff = runFn2 _forkAff nonCanceler aff

  -- | Promotes any error to the value level of the asynchronous monad.
  attempt :: forall e a. Aff e a -> Aff e (Either Error a)
  attempt aff = runFn3 _attempt Left Right aff

  -- | Ignores any errors.
  apathize :: forall e a. Aff e a -> Aff e Unit
  apathize a = const unit <$> attempt a

  -- | Lifts a synchronous computation and makes explicit any failure from exceptions.
  liftEff' :: forall e a. Eff (err :: Exception | e) a -> Aff e (Either Error a)
  liftEff' eff = attempt (_unsafeInterleaveAff (runFn2 _liftEff nonCanceler eff))

  -- | A constant function that always returns a pure false value.
  nonCanceler :: forall e. Canceler e
  nonCanceler = Canceler (const (pure false))

  alwaysCanceler :: forall e. Canceler e
  alwaysCanceler = Canceler (const (pure true))

  instance semigroupAff :: (Semigroup a) => Semigroup (Aff e a) where
    (<>) a b = (<>) <$> a <*> b

  instance monoidAff :: (Monoid a) => Monoid (Aff e a) where
    mempty = pure mempty

  instance functorAff :: Functor (Aff e) where
    (<$>) f fa = runFn2 _fmap f fa

  instance applyAff :: Apply (Aff e) where
    (<*>) ff fa = runFn3 _bind alwaysCanceler ff (\f -> f <$> fa)

  instance applicativeAff :: Applicative (Aff e) where
    pure v = runFn2 _pure nonCanceler v

  instance bindAff :: Bind (Aff e) where
    (>>=) fa f = runFn3 _bind alwaysCanceler fa f

  instance monadAff :: Monad (Aff e)

  instance monadEffAff :: MonadEff e (Aff e) where
    liftEff eff = runFn2 _liftEff nonCanceler eff

  -- | Allows users to catch and throw errors on the error channel of the
  -- | asynchronous computation. See documentation in `purescript-transformers`.
  instance monadErrorAff :: MonadError Error (Aff e) where
    throwError e = runFn2 _throwError nonCanceler e

    catchError aff ex = attempt aff >>= either ex pure

  instance altAff :: Alt (Aff e) where
    (<|>) a1 a2 = attempt a1 >>= either (const a2) pure

  instance plusAff :: Plus (Aff e) where
    empty = throwError $ error "Always fails"

  instance alternativeAff :: Alternative (Aff e)

  instance monadPlusAff :: MonadPlus (Aff e)

  instance semigroupCanceler :: Semigroup (Canceler e) where
    (<>) (Canceler f1) (Canceler f2) = Canceler (\e -> (||) <$> f1 e <*> f2 e)

  instance monoidCanceler :: Monoid (Canceler e) where
    mempty = Canceler (const (pure true))

  foreign import _cancelWith """
    function _cancelWith(nonCanceler, aff, canceler1) {
      return function(success, error) {
        var canceler2 = aff(success, error);

        return function(e) {
          return function(success, error) {
            var cancellations = 0;
            var result        = false;
            var errored       = false;

            var s = function(bool) {
              cancellations = cancellations + 1;
              result        = result || bool;

              if (cancellations === 2 && !errored) {
                try {
                  success(result);
                } catch (e) {
                  error(e);
                }
              }
            };

            var f = function(err) {
              if (!errored) {
                errored = true;

                error(err);
              }
            };

            canceler2(e)(s, f);
            canceler1(e)(s, f);            

            return nonCanceler;
          };
        };
      };
    }
  """ :: forall e a. Fn3 (Canceler e) (Aff e a) (Canceler e) (Aff e a)

  foreign import _setTimeout """
    function _setTimeout(nonCanceler, millis, aff) {
      return function(success, error) {
        var canceler;

        var timeout = setTimeout(function() {
          canceler = aff(success, error);
        }, millis);

        return function(e) {
          return function(s, f) {
            if (canceler !== undefined) {
              return canceler(e)(s, f);
            } else {
              clearTimeout(timeout);

              try {
                s(true);
              } catch (e) {
                f(e);
              }

              return nonCanceler;
            }
          };
        };
      };
    }
  """ :: forall e a. Fn3 (Canceler e) Number (Aff e a) (Aff e a)

  foreign import _unsafeInterleaveAff """
    function _unsafeInterleaveAff(aff) {
      return aff;
    }
  """ :: forall e1 e2 a. Aff e1 a -> Aff e2 a

  foreign import _forkAff """
    function _forkAff(nonCanceler, aff) {
      var voidF = function(){};

      return function(success, error) {
        var canceler = aff(voidF, voidF);

        try {
          success(canceler);
        } catch (e) {
          error(e);
        }

        return nonCanceler;
      };
    }
  """ :: forall e a. Fn2 (Canceler e) (Aff e a) (Aff e (Canceler e))

  foreign import _makeAff """
    function _makeAff(cb) {
      return function(success, error) {
        return cb(function(e) {
          return function() {
            error(e);
          };
        })(function(v) {
          return function() {
            try {
              success(v);
            } catch (e) {
              error(e);
            }
          };
        })();
      }
    }
    """ :: forall e a. ((Error -> Eff e Unit) -> (a -> Eff e Unit) -> Eff e (Canceler e)) -> Aff e a

  foreign import _pure """
    function _pure(canceler, v) {
      return function(success, error) {
        try {
          success(v);
        } catch (e) {
          error(e);
        }

        return canceler;
      }
    }""" :: forall e a. Fn2 (Canceler e) a (Aff e a)

  foreign import _throwError """
    function _throwError(canceler, e) {
      return function(success, error) {
        error(e);

        return canceler;
      };
    }""" :: forall e a. Fn2 (Canceler e) Error (Aff e a)

  foreign import _fmap """
    function _fmap(f, aff) {
      return function(success, error) {
        return aff(function(v) {
          try {
            success(f(v));
          } catch (e) {
            error(e);
          }
        }, error);
      };
    }""" :: forall e a b. Fn2 (a -> b) (Aff e a) (Aff e b)

  foreign import _bind """
    function _bind(alwaysCanceler, aff, f) {
      return function(success, error) {
        var canceler1, canceler2;

        var isCanceled    = false;
        var requestCancel = false;

        var onCanceler = function(){};

        canceler1 = aff(function(v) {
          if (requestCancel) {
            isCanceled = true;

            return alwaysCanceler;
          } else {
            canceler2 = f(v)(success, error);

            onCanceler(canceler2);

            return canceler2;
          }
        }, error);

        return function(e) {
          return function(s, f) {
            requestCancel = true;

            if (canceler2 !== undefined) {
              return canceler2(e)(s, f);
            } else {
              return canceler1(e)(function(bool) {
                if (bool || isCanceled) {
                  try {
                    s(true);
                  } catch (e) {
                    f(e);
                  }
                } else {
                  onCanceler = function(canceler) {
                    canceler(e)(s, f);
                  };
                }
              }, f);
            }
          };
        };
      };
    }""" :: forall e a b. Fn3 (Canceler e) (Aff e a) (a -> Aff e b) (Aff e b)

  foreign import _attempt """
    function _attempt(Left, Right, aff) {
      return function(success, error) {
        return aff(function(v) {
          try {
            success(Right(v));
          } catch (e) {
            error(e);
          }
        }, function(e) {
          try {
            success(Left(e));
          } catch (e) {
            error(e);
          }
        });
      };
    }"""  :: forall e a. Fn3 (forall x y. x -> Either x y) (forall x y. y -> Either x y) (Aff e a) (Aff e (Either Error a))

  foreign import _runAff """
    function _runAff(errorT, successT, aff) {
      return function() {
        return aff(function(v) {
          try {
            successT(v)();
          } catch (e) {
            errorT(e)();
          }
        }, function(e) {
          errorT(e)();
        });
      };
    }""" :: forall e a. Fn3 (Error -> Eff e Unit) (a -> Eff e Unit) (Aff e a) (Eff e Unit)

  foreign import _liftEff """
    function _liftEff(canceler, e) {
      return function(success, error) {
        try {
          success(e());
        } catch (e) {
          error(e);
        }

        return canceler;
      };
    }""" :: forall e a. Fn2 (Canceler e) (Eff e a) (Aff e a)



