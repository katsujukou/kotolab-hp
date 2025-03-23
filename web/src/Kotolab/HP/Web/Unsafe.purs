module Kotolab.HP.Web.Unsafe where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Web.HTML (HTMLElement)

foreign import innerHtmlImpl :: EffectFn2 HTMLElement String Unit

innerHtml :: HTMLElement -> String -> Effect Unit
innerHtml = runEffectFn2 innerHtmlImpl