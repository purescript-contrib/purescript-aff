-- | This module provides compatability functions for constructing `Aff`s which
-- | are defined via the FFI.
module Control.Monad.Aff.Compat
  ( EffFnAff(..)
  , EffFnCanceler(..)
  , EffFnCb
  , fromEffFnAff
  , module Control.Monad.Eff.Uncurried
  ) where

import Prelude
import Control.Monad.Aff (Aff, Canceler(..), makeAff, nonCanceler)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, mkEffFn1, mkEffFn2, mkEffFn3, runEffFn1, runEffFn2, runEffFn3)
import Data.Either (Either(..))

type EffFnCb eff a = EffFn1 eff a Unit

newtype EffFnAff eff a = EffFnAff (EffFn2 eff (EffFnCb eff Error) (EffFnCb eff a) (EffFnCanceler eff))

newtype EffFnCanceler eff = EffFnCanceler (EffFn3 eff Error (EffFnCb eff Error) (EffFnCb eff Unit) Unit)

-- | Lift a FFI definition into an `Aff`. `EffFnAff` makes use of `EffFn` so
-- | `Eff` thunks are unnecessary. A definition might follow this example:
-- |
-- | ```javascript
-- | exports._myAff = function (onError, onSuccess) {
-- |   var cancel = doSomethingAsync(function (err, res) {
-- |     if (err) {
-- |       onError(err);
-- |     } else {
-- |       onSuccess(res);
-- |     }
-- |   });
-- |   return function (cancelError, onCancelerError, onCancelerSuccess) {
-- |     cancel();
-- |     onCancelerSuccess();
-- |   };
-- | };
-- | ```
-- |
-- | ```purescript
-- | foreign import _myAff :: forall eff. EffFnAff (myeffect :: MYEFFECT | eff) String
-- |
-- | myAff :: forall eff. Aff (myeffect :: MYEFFECT | eff) String
-- | myAff = fromEffFnAff _myAff
-- | ````
fromEffFnAff ∷ ∀ eff a. EffFnAff eff a → Aff eff a
fromEffFnAff (EffFnAff eff) = makeAff \k → do
  EffFnCanceler canceler ← runEffFn2 eff (mkEffFn1 (k <<< Left)) (mkEffFn1 (k <<< Right))
  pure $ Canceler \e → makeAff \k2 → do
    runEffFn3 canceler e (mkEffFn1 (k2 <<< Left)) (mkEffFn1 (k2 <<< Right))
    pure nonCanceler
