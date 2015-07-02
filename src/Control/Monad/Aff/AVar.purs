-- | A low-level primitive for building asynchronous code.
module Control.Monad.Aff.AVar
  ( AffAVar()
  , AVar()
  , AVAR()
  , killVar
  , makeVar
  , makeVar'
  , modifyVar
  , putVar
  , takeVar
  ) where

  import Prelude
  import Data.Function(Fn2(), Fn3(), runFn2, runFn3)
  import Control.Monad.Aff
  import Control.Monad.Eff.Exception(Error())

  foreign import data AVAR :: !

  foreign import data AVar :: * -> *

  type AffAVar e a = Aff (avar :: AVAR | e) a

  -- | Makes a new asynchronous avar.
  makeVar :: forall e a. AffAVar e (AVar a)
  makeVar = _makeVar nonCanceler

  -- | Makes a avar and sets it to some value.
  makeVar' :: forall e a. a -> AffAVar e (AVar a)
  makeVar' a = do
    v <- makeVar 
    putVar v a
    return v

  -- | Takes the next value from the asynchronous avar.
  takeVar :: forall e a. AVar a -> AffAVar e a
  takeVar q = runFn2 _takeVar nonCanceler q

  -- | Puts a new value into the asynchronous avar. If the avar has
  -- | been killed, this will result in an error.
  putVar :: forall e a. AVar a -> a -> AffAVar e Unit
  putVar q a = runFn3 _putVar nonCanceler q a

  -- | Modifies the value at the head of the avar (will suspend until one is available).
  modifyVar :: forall e a. (a -> a) -> AVar a -> AffAVar e Unit
  modifyVar f v = takeVar v >>= (f >>> putVar v)

  -- | Kills an asynchronous avar.
  killVar :: forall e a. AVar a -> Error -> AffAVar e Unit
  killVar q e = runFn3 _killVar nonCanceler q e

  foreign import _makeVar :: forall e a. Canceler e -> AffAVar e (AVar a)

  foreign import _takeVar :: forall e a. Fn2 (Canceler e) (AVar a) (AffAVar e a)
  
  foreign import _putVar :: forall e a. Fn3 (Canceler e) (AVar a) a (AffAVar e Unit)

  foreign import _killVar :: forall e a. Fn3 (Canceler e) (AVar a) Error (AffAVar e Unit)

