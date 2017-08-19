module Control.Monad.Aff.Console
  ( module Exports
  , log
  , logShow
  , warn
  , warnShow
  , error
  , errorShow
  , info
  , infoShow
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE) as Exports
import Control.Monad.Eff.Console as C

-- | Write a message to the console. Shorthand for `liftEff $ log x`.
log ∷ ∀ eff. String → Aff (console ∷ C.CONSOLE | eff) Unit
log = liftEff <<< C.log

-- | Write a value to the console, using its `Show` instance to produce a
-- | `String`. Shorthand for `liftEff $ logShow x`.
logShow ∷ ∀ a eff. Show a ⇒ a → Aff (console ∷ C.CONSOLE | eff) Unit
logShow = liftEff <<< C.logShow

-- | Write a warning to the console. Shorthand for `liftEff $ warn x`.
warn ∷ ∀ eff. String → Aff (console ∷ C.CONSOLE | eff) Unit
warn = liftEff <<< C.warn

-- | Write a warning value to the console, using its `Show` instance to produce
-- | a `String`. Shorthand for `liftEff $ warnShow x`.
warnShow ∷ ∀ a eff. Show a ⇒ a → Aff (console ∷ C.CONSOLE | eff) Unit
warnShow = liftEff <<< C.warnShow

-- | Write an error to the console. Shorthand for `liftEff $ error x`.
error ∷ ∀ eff. String → Aff (console ∷ C.CONSOLE | eff) Unit
error = liftEff <<< C.error

-- | Write an error value to the console, using its `Show` instance to produce a
-- | `String`. Shorthand for `liftEff $ errorShow x`.
errorShow ∷ ∀ a eff. Show a ⇒ a → Aff (console ∷ C.CONSOLE | eff) Unit
errorShow = liftEff <<< C.errorShow

-- | Write an info message to the console. Shorthand for `liftEff $ info x`.
info ∷ ∀ eff. String → Aff (console ∷ C.CONSOLE | eff) Unit
info = liftEff <<< C.info

-- | Write an info value to the console, using its `Show` instance to produce a
-- | `String`. Shorthand for `liftEff $ infoShow x`.
infoShow ∷ ∀ a eff. Show a ⇒ a → Aff (console ∷ C.CONSOLE | eff) Unit
infoShow = liftEff <<< C.infoShow
