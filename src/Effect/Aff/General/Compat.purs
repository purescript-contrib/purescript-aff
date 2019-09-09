-- | This module provides compatability functions for constructing `Aff`s which
-- | are defined via the FFI.
module Effect.Aff.General.Compat
  ( EffectFnAff(..)
  , EffectFnCanceler(..)
  , EffectFnCb
  , fromEffectFnAff
  , module Effect.Uncurried
  ) where

import Prelude

import Effect.Aff.General (Aff, AffResult(..), Canceler(..), catch, makeAff, nonCanceler, panic)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, mkEffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)

type EffectFnCb a = EffectFn1 a Unit

newtype EffectFnAff e a = EffectFnAff (EffectFn3 (EffectFnCb e) (EffectFnCb a) (EffectFnCb Error) EffectFnCanceler)

newtype EffectFnCanceler = EffectFnCanceler (EffectFn3 Error (EffectFnCb Error) (EffectFnCb Unit) Unit)

-- | Lift a FFI definition into an `Aff`. `EffectFnAff` makes use of `EffectFn` so
-- | `Effect` thunks are unnecessary. A definition might follow this example:
-- |
-- | ```javascript
-- | exports._myAff = function (onError, onSuccess) {
-- |   var cancel = doSomethingAsync(function (err, res, panic) {
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
-- | foreign import _myAff :: EffectFnAff String
-- |
-- | myAff :: Aff String
-- | myAff = fromEffectFnAff _myAff
-- | ````
fromEffectFnAff ∷ ∀ e. EffectFnAff e ~> Aff e
fromEffectFnAff (EffectFnAff eff) = makeAff \k → do
  EffectFnCanceler canceler ←
    runEffectFn3 eff (mkEffectFn1 (k <<< Failed))
                     (mkEffectFn1 (k <<< Succeeded))
                     (mkEffectFn1 (k <<< Interrupted))
  pure $ Canceler \e →
    catch
    ( makeAff \k2 → ado
         runEffectFn3 canceler e (mkEffectFn1 (k2 <<< Interrupted))
                                 (mkEffectFn1 (k2 <<< Succeeded))
      in nonCanceler
    )
    panic
