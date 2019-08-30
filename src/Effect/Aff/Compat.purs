module Effect.Aff.Compat
  ( EffectFnAff
  , EffectFnCanceler
  , fromEffectFnAff
  , module Effect.Uncurried
  , module Exports
  ) where

import Effect.Aff (Aff)
import Effect.Aff.General.Compat (EffectFnCb) as Exports
import Effect.Aff.General.Compat as G
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, mkEffectFn2, mkEffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Prelude (type (~>))

type EffectFnAff = G.EffectFnAff Error

type EffectFnCanceler = G.EffectFnCanceler

fromEffectFnAff ∷ EffectFnAff ~> Aff
fromEffectFnAff = G.fromEffectFnAff
