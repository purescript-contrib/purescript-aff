-- | A low-level primitive for building asynchronous code.
module Control.Monad.Aff.AVar
  ( AffAVar
  , AVAR
  , makeVar
  , makeVar'
  , takeVar
  , peekVar
  , putVar
  , modifyVar
  , killVar
  , module Exports
  ) where

import Prelude

import Control.Monad.Aff (Aff, nonCanceler)
import Control.Monad.Aff.Internal (AVar) as Exports
import Control.Monad.Aff.Internal (AVBox, AVar, _killVar, _putVar, _takeVar, _peekVar, _makeVar)
import Control.Monad.Eff.Exception (Error())

import Data.Function.Uncurried (runFn3, runFn2)

import Unsafe.Coerce (unsafeCoerce)

foreign import data AVAR :: !

type AffAVar e a = Aff (avar :: AVAR | e) a

-- | Makes a new asynchronous avar.
makeVar :: forall e a. AffAVar e (AVar a)
makeVar = fromAVBox $ _makeVar nonCanceler

-- | Makes a avar and sets it to some value.
makeVar' :: forall e a. a -> AffAVar e (AVar a)
makeVar' a = do
  v <- makeVar
  putVar v a
  pure v

-- | Takes the next value from the asynchronous avar.
takeVar :: forall e a. AVar a -> AffAVar e a
takeVar q = fromAVBox $ runFn2 _takeVar nonCanceler q

-- | Reads a value from the asynchronous var but does not consume it.
peekVar :: forall e a. AVar a -> AffAVar e a
peekVar q = fromAVBox $ runFn2 _peekVar nonCanceler q

-- | Puts a new value into the asynchronous avar. If the avar has
-- | been killed, this will result in an error.
putVar :: forall e a. AVar a -> a -> AffAVar e Unit
putVar q a = fromAVBox $ runFn3 _putVar nonCanceler q a

-- | Modifies the value at the head of the avar (will suspend until one is available).
modifyVar :: forall e a. (a -> a) -> AVar a -> AffAVar e Unit
modifyVar f v = takeVar v >>= (f >>> putVar v)

-- | Kills an asynchronous avar.
killVar :: forall e a. AVar a -> Error -> AffAVar e Unit
killVar q e = fromAVBox $ runFn3 _killVar nonCanceler q e

fromAVBox :: forall a e. AVBox a -> AffAVar e a
fromAVBox = unsafeCoerce
