module Control.Monad.Aff.Internal
  ( AVBox
  , AVar
  , _makeVar
  , _takeVar
  , _tryTakeVar
  , _peekVar
  , _tryPeekVar
  , _putVar
  , _killVar
  ) where

import Prelude

import Control.Monad.Eff.Exception (Error)

import Data.Maybe (Maybe)
import Data.Function.Uncurried (Fn2, Fn3, Fn4)

foreign import data AVar :: Type -> Type

foreign import data AVBox :: Type -> Type

foreign import _makeVar :: forall c a. c -> AVBox (AVar a)

foreign import _takeVar :: forall c a. Fn2 c (AVar a) (AVBox a)

foreign import _tryTakeVar :: forall c a. Fn4 (forall x. Maybe x) (forall x. x -> Maybe x) c (AVar a) (AVBox (Maybe a))

foreign import _peekVar :: forall c a. Fn2 c (AVar a) (AVBox a)

foreign import _tryPeekVar :: forall c a. Fn4 (forall x. Maybe x) (forall x. x -> Maybe x) c (AVar a) (AVBox (Maybe a))

foreign import _putVar :: forall c a. Fn3 c (AVar a) a (AVBox Unit)

foreign import _killVar :: forall c a. Fn3 c (AVar a) Error (AVBox Unit)
