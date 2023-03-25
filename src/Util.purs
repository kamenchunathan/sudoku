module Util (consoleLog) where

import Prelude

import Effect (Effect)
import Effect.Aff.Compat (runEffectFn1)
import Effect.Uncurried (EffectFn1)

foreign import _consoleLog :: forall a. EffectFn1 a Unit

consoleLog :: forall a. a -> Effect Unit
consoleLog = runEffectFn1 _consoleLog

