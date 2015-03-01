module Control.Monad.Aff 
  ( Aff()
  , Async()
  , EffA()
  , PureAff(..)
  , attempt
  , catch
  , catch'
  , launchAff
  , makeAff
  , makeAff'
  , runAff
  , throw
  )
  where 

  import Data.Either(Either(..))
  import Data.Monoid(Monoid, mempty)
  import Control.Apply
  import Control.Monad.Eff
  import Control.Monad.Eff.Exception(Error(), Exception(), catchException)
  import Control.Monad.Eff.Unsafe(unsafeInterleaveEff)
  import Control.Monad.Eff.Class

  -- | An effect constructor which accepts a row of effects. `Async e` 
  -- | represents the effect of asynchronous computations which perform effects 
  -- | `e` at some indeterminate point in the future.
  foreign import data Async :: # ! -> !

  -- | The `Eff` type for a computation which has asynchronous effects `e1` and
  -- | synchronous effects `e2`.
  type EffA e1 e2 a = Eff (async :: Async e1 | e2) a

  -- | A computation with asynchronous effects `e1` and synchronous effects 
  -- | `e2`. The computation either errors or produces a value of type `a`.
  -- |
  -- | This is moral equivalent of `ErrorT (ContT Unit (EffA e1 e2)) a`.
  -- |
  -- | The type implements `MonadEff` so it's easy to lift synchronous `Eff` 
  -- | computations into this type. As a result, there's basically no reason to
  -- | use `Eff` in a program that has some asynchronous computations.
  data Aff e1 e2 a = Aff ((Error -> Eff e1 Unit) -> (a -> Eff e1 Unit) -> EffA e1 e2 Unit)

  type PureAff a = forall e1 e2. Aff e1 e2 a

  -- | Converts the asynchronous effect into a synchronous one. All values and
  -- | errors are ignored.
  launchAff :: forall e1 e2 a. Aff e1 e2 a -> EffA e1 e2 Unit
  launchAff (Aff fa) = fa (const (pure unit)) (const (pure unit))

  -- | Runs the asynchronous effect. You must supply an error callback and a 
  -- | success callback.
  runAff :: forall e1 e2 a. (Error -> Eff e1 Unit) -> (a -> Eff e1 Unit) -> Aff e1 e2 a -> EffA e1 e2 Unit
  runAff ex f (Aff v) = v ex f

  -- | Creates an asynchronous effect from a function that accepts error and 
  -- | success callbacks.
  makeAff :: forall e1 e2 a. ((Error -> Eff e1 Unit) -> (a -> Eff e1 Unit) -> EffA e1 e2 Unit) -> Aff e1 e2 a
  makeAff = Aff

  makeAff' :: forall e1 e2 a. ((Error -> Eff e1 Unit) -> (a -> Eff e1 Unit) -> Eff e2 Unit) -> Aff e1 e2 a
  makeAff' h = Aff (\ex f -> unsafeInterleaveEff (h ex f))

  -- | Promotes any error to the value level of the asynchronous monad.
  attempt :: forall e1 e2 a. Aff e1 e2 a -> Aff e1 e2 (Either Error a)
  attempt (Aff fa) = Aff (\_ f -> fa (Left >>> f) (Right >>> f))

  -- | Removes synchronous exceptions by forcing them through the error callback.
  -- | In order for this to work, the effects of the async computation must match
  -- | the effects of the error callback. If this isn't the case, you can explicitly
  -- | deal with the exception using `catch'`.
  catch :: forall e a. Aff e (err :: Exception | e) a -> Aff e e a
  catch (Aff fa) = Aff (\ex f -> catchException (unsafeInterleaveEff <$> ex) (fa ex f))

  -- | Removes synchronous exceptions by passing them to the specified handler.
  -- | The handler is allowed the same class of effects as the synchronous part
  -- | of the asynchronous computation.
  catch' :: forall e1 e2 a. (Error -> Eff e2 Unit) -> Aff e1 (err :: Exception | e2) a -> Aff e1 e2 a
  catch' ex0 (Aff fa) = Aff (\ex f -> catchException (unsafeInterleaveEff <$> ex0) (fa ex f))

  -- | Throws the specified exception through the error callback.
  throw :: forall e1 e2 a. Error -> Aff e1 e2 a
  throw e = Aff (\ex _ -> unsafeInterleaveEff (ex e))

  instance semigroupAff :: (Semigroup a) => Semigroup (Aff e1 e2 a) where
    (<>) a b = (<>) <$> a <*> b

  instance monoidAff :: (Monoid a) => Monoid (Aff e1 e2 a) where
    mempty = pure mempty

  instance functorAff :: Functor (Aff e1 e2) where
    (<$>) f (Aff fa) = Aff (\ex h -> fa ex (\a -> h (f a)))

  instance applyAff :: Apply (Aff e1 e2) where
    (<*>) (Aff ff) (Aff fa) = Aff (\ex h -> ff ex (\f -> unsafeInterleaveEff (fa ex (\a -> h (f a)))))

  instance applicativeAff :: Applicative (Aff e1 e2) where
    pure v = Aff (\_ h -> unsafeInterleaveEff (h v))

  instance bindAff :: Bind (Aff e1 e2) where
    (>>=) (Aff fa) f = Aff (\ex h -> fa ex (\a -> unsafeInterleaveEff (runAff ex (\b -> h b) (f a))))

  instance monadAff :: Monad (Aff e1 e2)

  instance monadEffAff :: MonadEff e2 (Aff e1 e2) where
    liftEff fa = Aff (\_ h -> unsafeInterleaveEff (unsafeInterleaveEff fa >>= h))
