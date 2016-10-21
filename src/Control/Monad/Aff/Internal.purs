module Control.Monad.Aff.Internal
  ( AVBox
  , AVar
  , _makeVar
  , _takeVar
  , _peekVar
  , _putVar
  , _killVar
  ) where

import Prelude

import Control.Monad.Eff.Exception (Error)

import Data.Function.Uncurried (Fn2, Fn3)

foreign import data AVar :: * -> *

foreign import data AVBox :: * -> *

foreign import _makeVar :: forall c a. c -> AVBox (AVar a)

foreign import _takeVar :: forall c a. Fn2 c (AVar a) (AVBox a)

foreign import _peekVar :: forall c a. Fn2 c (AVar a) (AVBox a)

foreign import _putVar :: forall c a. Fn3 c (AVar a) a (AVBox Unit)

foreign import _killVar :: forall c a. Fn3 c (AVar a) Error (AVBox Unit)
