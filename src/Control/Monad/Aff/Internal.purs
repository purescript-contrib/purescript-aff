module Control.Monad.Aff.Internal
  ( Aff
  , AffModality
  , ParAff(..)
  , Thread(..)
  , Canceler(..)
  , ASYNC
  , nonCanceler
  , makeAff
  , launchAff
  , attempt
  , bracket
  , delay
  , unsafeLaunchAff
  , unsafeLiftEff
  , unsafeMakeAff
  ) where

import Prelude
import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error)
import Control.Monad.Eff.Ref (newRef, readRef, writeRef)
import Control.Monad.Eff.Ref.Unsafe (unsafeRunRef)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Parallel (parSequence_)
import Control.Parallel.Class (class Parallel)
import Control.Plus (class Plus, empty)
import Data.Either (Either(..), isLeft)
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds(..))
import Partial.Unsafe (unsafeCrashWith)
import Type.Row.Effect.Equality (class EffectRowEquals, effTo)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Aff ∷ # Effect → Type → Type

foreign import data ASYNC ∷ Effect

type AffModality eff =
  ( exception ∷ EXCEPTION
  , async ∷ ASYNC
  | eff
  )

instance functorAff ∷ Functor (Aff eff) where map = _map
instance applyAff ∷ Apply (Aff eff) where apply = ap
instance applicativeAff ∷ Applicative (Aff eff) where pure = _pure
instance bindAff ∷ Bind (Aff eff) where bind = _bind
instance monadAff ∷ Monad (Aff eff)

instance semigroupAff ∷ Semigroup a ⇒ Semigroup (Aff eff a) where
  append = lift2 append

instance monoidAff ∷ Monoid a ⇒ Monoid (Aff eff a) where
  mempty = pure mempty

instance altAff ∷ Alt (Aff eff) where
  alt a1 a2 = do
    res ← attempt a1
    case res of
      Left err → a2
      Right r  → pure r

instance plusAff ∷ Plus (Aff eff) where
  empty = throwError (error "Always fails")

instance alternativeAff ∷ Alternative (Aff eff)

instance monadZeroAff ∷ MonadZero (Aff eff)

instance monadPlusAff ∷ MonadPlus (Aff eff)

instance monadRecAff ∷ MonadRec (Aff eff) where
  tailRecM k = go
    where
    go a = do
      res ← k a
      case res of
        Done r → pure r
        Loop b → go b

instance monadThrowAff ∷ MonadThrow Error (Aff eff) where
  throwError = _throwError

instance monadErrorAff ∷ MonadError Error (Aff eff) where
  catchError aff k = do
    res ← attempt aff
    case res of
      Left err → k err
      Right r  → pure r

instance monadEffAff ∷ EffectRowEquals eff1 (exception ∷ EXCEPTION, async ∷ ASYNC | eff2) ⇒ MonadEff eff1 (Aff eff2) where
  liftEff eff = Fn.runFn3 _liftEff Left Right (effTo eff)

newtype ParAff eff a = ParAff (Aff eff a)

derive instance newtypeParAff ∷ Newtype (ParAff eff a) _
derive newtype instance functorParAff ∷ Functor (ParAff eff)

instance applyParAff ∷ Apply (ParAff eff) where
  apply (ParAff ff) (ParAff fa) = ParAff (unsafeMakeAff go)
    where
    go k = do
      Thread t1 ← unsafeLaunchAff ff
      Thread t2 ← unsafeLaunchAff fa
      Thread t3 ← unsafeLaunchAff do
        f ← attempt t1.join
        a ← attempt t2.join
        unsafeLiftEff (Right <$> k (f <*> a))
      pure $ Canceler \err →
        parSequence_
          [ t3.kill err
          , t1.kill err
          , t2.kill err
          ]

instance applicativeParAff ∷ Applicative (ParAff eff) where
  pure = ParAff <<< pure

instance semigroupParAff ∷ Semigroup a ⇒ Semigroup (ParAff eff a) where
  append = lift2 append

instance monoidParAff ∷ Monoid a ⇒ Monoid (ParAff eff a) where
  mempty = pure mempty

instance altParAff ∷ Alt (ParAff eff) where
  alt (ParAff a1) (ParAff a2) = ParAff (unsafeMakeAff go)
    where
    go k = do
      ref ← unsafeRunRef $ newRef Nothing
      Thread t1 ← unsafeLaunchAff a1
      Thread t2 ← unsafeLaunchAff a2

      let
        lift ∷ ∀ a. Eff eff a → Aff eff a
        lift = unsafeLiftEff <<< map Right

        earlyError =
          error "Alt ParAff: early exit"

        runK t r = do
          res ← lift $ unsafeRunRef $ readRef ref
          case res, r of
            Nothing, Left _ → lift $ unsafeRunRef $ writeRef ref (Just r)
            Nothing, Right _ → t.kill earlyError *> lift (k r)
            Just r', _ → t.kill earlyError *> lift (k r')

      Thread t3 ← unsafeLaunchAff $ runK t2 =<< attempt t1.join
      Thread t4 ← unsafeLaunchAff $ runK t1 =<< attempt t2.join

      pure $ Canceler \err →
        parSequence_
          [ t3.kill earlyError
          , t4.kill earlyError
          , t1.kill earlyError
          , t2.kill earlyError
          ]

instance plusParAff ∷ Plus (ParAff e) where
  empty = ParAff empty

instance alternativeParAff ∷ Alternative (ParAff e)

instance parallelAff ∷ Parallel (ParAff eff) (Aff eff) where
  parallel = ParAff
  sequential (ParAff aff) = aff

newtype Thread eff a = Thread
  { kill ∷ Error → Aff eff Unit
  , join ∷ Aff eff a
  }

instance functorThread ∷ Functor (Thread eff) where
  map f (Thread { kill, join }) = Thread { kill, join: f <$> join }

newtype Canceler eff = Canceler (Error → Aff eff Unit)

derive instance newtypeCanceler ∷ Newtype (Canceler eff) _

instance semigroupCanceler ∷ Semigroup (Canceler eff) where
  append (Canceler c1) (Canceler c2) =
    Canceler \err → parSequence_ [ c1 err, c2 err ]

instance monoidCanceler ∷ Monoid (Canceler eff) where
  mempty = nonCanceler

nonCanceler ∷ ∀ eff. Canceler eff
nonCanceler = Canceler k where k _ = pure unit

launchAff ∷ ∀ eff a. Aff eff a → Eff (async ∷ ASYNC | eff) (Thread eff a)
launchAff aff = Fn.runFn6 _launchAff isLeft unsafeFromLeft unsafeFromRight Left Right aff

unsafeLaunchAff ∷ ∀ eff a. Aff eff a → Eff eff (Thread eff a)
unsafeLaunchAff = unsafeCoerce launchAff

makeAff ∷ ∀ eff a. ((Either Error a → Eff (AffModality eff) Unit) → Eff (AffModality eff) (Canceler eff)) → Aff eff a
makeAff k = Fn.runFn3 _makeAff Left Right k

delay ∷ ∀ eff. Milliseconds → Aff eff Unit
delay (Milliseconds n) = Fn.runFn2 _delay Right n

foreign import _pure ∷ ∀ eff a. a → Aff eff a
foreign import _throwError ∷ ∀ eff a. Error → Aff eff a
foreign import _map ∷ ∀ eff a b. (a → b) → Aff eff a → Aff eff b
foreign import _bind ∷ ∀ eff a b. Aff eff a → (a → Aff eff b) → Aff eff b
foreign import _delay ∷ ∀ a eff. Fn.Fn2 (Unit → Either a Unit) Number (Aff eff Unit)
foreign import attempt ∷ ∀ eff a. Aff eff a → Aff eff (Either Error a)
foreign import bracket ∷ ∀ eff a b. Aff eff a → (a → Aff eff Unit) → (a → Aff eff b) → Aff eff b
foreign import unsafeLiftEff ∷ ∀ eff a. Eff eff (Either Error a) → Aff eff a
foreign import unsafeMakeAff ∷ ∀ eff a. ((Either Error a → Eff eff Unit) → Eff eff (Canceler eff)) → Aff eff a

foreign import _liftEff
  ∷ ∀ eff a
  . Fn.Fn3
      (Error → Either Error a)
      (a → Either Error a)
      (Eff (AffModality eff) a)
      (Aff eff a)

foreign import _makeAff
  ∷ ∀ eff a
  . Fn.Fn3
      (Error → Either Error a)
      (a → Either Error a)
      ((Either Error a → Eff (AffModality eff) Unit) → Eff (AffModality eff) (Canceler eff))
      (Aff eff a)

foreign import _launchAff
  ∷ ∀ eff a
  . Fn.Fn6
      (Either Error a → Boolean)
      (Either Error a → Error)
      (Either Error a → a)
      (Error → Either Error a)
      (a → Either Error a)
      (Aff eff a)
      (Eff (async ∷ ASYNC | eff) (Thread eff a))

unsafeFromLeft ∷ ∀ x y. Either x y → x
unsafeFromLeft = case _ of
  Left a → a
  Right  _ → unsafeCrashWith "unsafeFromLeft: Right"

unsafeFromRight ∷ ∀ x y. Either x y → y
unsafeFromRight = case _ of
  Right a → a
  Left  _ → unsafeCrashWith "unsafeFromRight: Left"
