module Control.Monad.Aff 
  ( Aff()
  , Async()
  , EffA()
  , PureAff(..)
  , apathize
  , attempt
  , forkAff
  , later
  , launchAff
  , liftEff'
  , makeAff
  , runAff
  )
  where 

  import Data.Either(Either(..), either)
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

  -- | The effect of being asynchronous.
  foreign import data Async :: !

  -- | The `Eff` type for a computation which has asynchronous effects.
  type EffA e a = Eff (async :: Async | e) a

  -- | A computation with effects `e`. The computation either errors or 
  -- | produces a value of type `a`.
  -- |
  -- | This is moral equivalent of `ErrorT (ContT Unit (EffA e)) a`.
  newtype Aff e a = Aff ((Error -> Eff e Unit) -> (a -> Eff e Unit) -> EffA e Unit)

  type PureAff a = forall e. Aff e a

  -- | Converts the asynchronous computation into a synchronous one. All values 
  -- | and errors are ignored.
  launchAff :: forall e a. Aff e a -> EffA e Unit
  launchAff (Aff fa) = fa (const (pure unit)) (const (pure unit))

  -- | Runs the asynchronous computation. You must supply an error callback and a 
  -- | success callback.
  runAff :: forall e a. (Error -> Eff e Unit) -> (a -> Eff e Unit) -> Aff e a -> EffA e Unit
  runAff ex f (Aff v) = v ex f

  -- | Creates an asynchronous effect from a function that accepts error and 
  -- | success callbacks.
  makeAff :: forall e a. ((Error -> Eff e Unit) -> (a -> Eff e Unit) -> EffA e Unit) -> Aff e a
  makeAff = Aff

  -- | Runs the asynchronous computation later.
  foreign import later """
    function later(aff) {
      return function(error) {
        return function(success) {
          return function() {
            setTimeout(aff(error)(success), 0);
          }
        }
      }
    }
  """ :: forall e a. Aff e a -> Aff e a

  -- | Forks the specified asynchronous computation so subsequent monadic binds 
  -- | will not block on the result of the computation.
  foreign import forkAff """
    function forkAff(aff) {
      return function(error) {
        return function(success) {
          return function() {
            try {
              var v = function(){return function(){};};

              aff(v)(v)();
            } catch (e) {
              error(e)();

              return;
            }

            success({})();
          }
        }
      }
    }
  """ :: forall e a. Aff e a -> Aff e Unit

  -- | Promotes any error to the value level of the asynchronous monad.
  attempt :: forall e a. Aff e a -> Aff e (Either Error a)
  attempt (Aff fa) = Aff (\_ f -> fa (Left >>> f) (Right >>> f))

  -- | Ignores any errors.
  apathize :: forall e a. Aff e a -> Aff e Unit
  apathize a = const unit <$> attempt a

  -- | Lifts a synchronous computation and makes explicit any failure from exceptions.
  liftEff' :: forall e a. Eff (err :: Exception | e) a -> Aff e (Either Error a)
  liftEff' eff = Aff (\_ f -> unsafeInterleaveEff (unsafeInterleaveEff (catchException (pure <$> Left) (Right <$> eff)) >>= f))

  instance semigroupAff :: (Semigroup a) => Semigroup (Aff e a) where
    (<>) a b = (<>) <$> a <*> b

  instance monoidAff :: (Monoid a) => Monoid (Aff e a) where
    mempty = pure mempty

  instance functorAff :: Functor (Aff e) where
    (<$>) f (Aff fa) = Aff (\ex h -> fa ex (\a -> h (f a)))

  instance applyAff :: Apply (Aff e) where
    (<*>) (Aff ff) (Aff fa) = Aff (\ex h -> ff ex (\f -> unsafeInterleaveEff (fa ex (\a -> h (f a)))))

  instance applicativeAff :: Applicative (Aff e) where
    pure v = Aff (\_ h -> unsafeInterleaveEff (h v))

  instance bindAff :: Bind (Aff e) where
    (>>=) (Aff fa) f = Aff (\ex h -> fa ex (\a -> unsafeInterleaveEff (runAff ex (\b -> h b) (f a))))

  instance monadAff :: Monad (Aff e)

  instance monadEffAff :: MonadEff e (Aff e) where
    liftEff fa = Aff (\_ h -> unsafeInterleaveEff (unsafeInterleaveEff fa >>= h))

  -- | Allows users to catch and throw errors on the error channel of the 
  -- | asynchronous computation. See documentation in `purescript-transformers`.
  instance monadErrorAff :: MonadError Error (Aff e) where
    throwError e = Aff (\ex _ -> unsafeInterleaveEff (ex e))

    catchError aff ex = attempt aff >>= either ex pure

  instance altAff :: Alt (Aff e) where
    (<|>) a1 a2 = attempt a1 >>= either (const a2) pure

  instance plusAff :: Plus (Aff e) where
    empty = throwError $ error "Always fails"

  instance alternativeAff :: Alternative (Aff e)

  instance monadPlusAff :: MonadPlus (Aff e)