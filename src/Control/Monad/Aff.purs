module Control.Monad.Aff 
  ( Aff(),
    PureAff(..),
    attempt,
    catch,
    launchAff,
    makeAff,
    runAff
  )
  where 

  import Data.Either(Either(..))
  import Data.Monoid(Monoid, mempty)
  import Control.Apply
  import Control.Monad.Eff
  import Control.Monad.Eff.Exception(Error(), Exception(), catchException)
  import Control.Monad.Eff.Unsafe(unsafeInterleaveEff)
  import Control.Monad.Eff.Class

  -- | An asynchronous computation with effects `e`, which either errors or 
  -- | produces a value of type `a`.
  -- |
  -- | This is the moral equivalent of `ErrorT (ContT Unit (Eff e)) a`, but 
  -- | faster, easier to use, and self-contained.
  -- |
  -- | The type implements `MonadEff` so it's easy to lift synchronous `Eff` 
  -- | computations into this type. As a result, there's basically no reason to
  -- | use `Eff` in a program that has some asynchronous computations.
  data Aff e a = Aff ((Error -> Eff e Unit) -> (a -> Eff e Unit) -> Eff e Unit)

  type PureAff a = forall e. Aff e a

  -- | Converts the asynchronous effect into a synchronous one. All values and
  -- | errors are ignored.
  launchAff :: forall e a. Aff e a -> Eff e Unit
  launchAff (Aff fa) = fa (const (pure unit)) (const (pure unit))

  -- | Runs the asynchronous effect. You must supply an error callback and a 
  -- | success callback.
  runAff :: forall e a. (Error -> Eff e Unit) -> (a -> Eff e Unit) -> Aff e a -> Eff e Unit
  runAff ex f (Aff v) = v ex f

  -- | Creates an asynchronous effect from a function that accepts error and 
  -- | success callbacks.
  makeAff :: forall e a. ((Error -> Eff e Unit) -> (a -> Eff e Unit) -> Eff e Unit) -> Aff e a
  makeAff = Aff

  -- | Promotes any error to the value level of the asynchronous monad.
  attempt :: forall e a. Aff e a -> Aff e (Either Error a)
  attempt (Aff fa) = Aff (\_ h -> fa (Left >>> h) (Right >>> h))

  -- | Removes exceptions by forcing them through the error callback.
  catch :: forall e a. Aff (err :: Exception | e) a -> Aff e a
  catch (Aff fa) = Aff (\ex h -> catchException ex (fa (unsafeInterleaveEff <$> ex) (\a -> unsafeInterleaveEff (h a))))

  instance semigroupAff :: (Semigroup a) => Semigroup (Aff e a) where
    (<>) a b = (<>) <$> a <*> b

  instance monoidAff :: (Monoid a) => Monoid (Aff e a) where
    mempty = pure mempty

  instance functorAff :: Functor (Aff e) where
    (<$>) f (Aff fa) = Aff (\ex h -> fa ex (\a -> h (f a)))

  instance applyAff :: Apply (Aff e) where
    (<*>) (Aff ff) (Aff fa) = Aff (\ex h -> ff ex (\f -> fa ex (\a -> h (f a))))

  instance applicativeAff :: Applicative (Aff e) where
    pure v = Aff (\_ h -> h v)

  instance bindAff :: Bind (Aff e) where
    (>>=) (Aff fa) f = Aff (\ex h -> fa ex (\a -> runAff ex (\b -> h b) (f a)))

  instance monadAff :: Monad (Aff e)

  instance monadEffAff :: MonadEff e (Aff e) where
    liftEff fa = Aff (\_ h -> fa >>= h)
