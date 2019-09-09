module Effect.Aff.Compat
  ( EffectFnAff
  , EffectFnCanceler
  , fromEffectFnAff
  , module Effect.Uncurried
  , module Exports
  ) where

import Effect.Aff (Aff, Error)
import Effect.Aff.General.Compat (EffectFnCb) as Exports
import Effect.Aff.General.Compat as G
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, mkEffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Prelude (type (~>))

type EffectFnCb a = G.EffectFnCb a

newtype EffectFnAff a = EffectFnAff
  (EffectFn2 (EffectFnCb Error) (EffectFnCb a) EffectFnCanceler)

type EffectFnCanceler = G.EffectFnCanceler

fromEffectFnAff ∷ EffectFnAff ~> Aff
fromEffectFnAff (EffectFnAff f) =
  G.fromEffectFnAff (G.EffectFnAff (mkEffectFn3 \a b _ → runEffectFn2 f a b))
