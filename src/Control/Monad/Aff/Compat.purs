-- | This module provides compatability functions for constructing `Aff`s which
-- | are defined via the FFI.
module Control.Monad.Aff.Compat
  ( EffFnAff(..)
  , EffFnCanceler(..)
  , fromEffFnAff
  ) where

import Prelude
import Control.Monad.Aff (Aff, Canceler(..), makeAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Uncurried as Fn
import Data.Either (Either(..))

newtype EffFnAff eff a = EffFnAff (Fn.EffFn2 eff (Fn.EffFn1 eff Error Unit) (Fn.EffFn1 eff a Unit) (EffFnCanceler eff))

newtype EffFnCanceler eff = EffFnCanceler (Fn.EffFn1 eff Error (EffFnAff eff Unit))

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
-- |   return function (cancelError) {
-- |     return function (onCancelerError, onCancelerSuccess) {
-- |       cancel();
-- |       onCancelerSuccess();
-- |     };
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
  EffFnCanceler canceler ← Fn.runEffFn2 eff (Fn.mkEffFn1 (k <<< Left)) (Fn.mkEffFn1 (k <<< Right))
  pure $ Canceler \e → fromEffFnAff =<< liftEff (Fn.runEffFn1 canceler e)
