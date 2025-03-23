module Kotolab.HP.Web.Component.PureScriptBadge where

import Prelude

import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect.Class.Console as Console
import Fmt as Fmt
import Halogen (RefLabel(..), liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (getRef, useLifecycleEffect)
import Halogen.Hooks as Hooks
import Kotolab.HP.Web.Capabilities.MonadAjax (class MonadAjax, sendRequest)
import Kotolab.HP.Web.Unsafe as Unsafe
import Web.HTML.HTMLElement as HTMLElement

type Input =
  { version :: String
  }

make :: forall q o m. MonadAjax m => H.Component q Input o m
make = Hooks.component \_ inps -> Hooks.do
  useLifecycleEffect do
    let
      url = Fmt.fmt
        @"https://img.shields.io/badge/purs-{ver}-blue?logo=purescript"
        { ver: inps.version
        }
    res <- sendRequest GET url Nothing []
    Console.log res
    mbEl <- map (HTMLElement.fromElement =<< _) $ getRef (RefLabel "badge-svg")
    case mbEl of
      Nothing -> pure unit
      Just el -> do
        liftEffect $ Unsafe.innerHtml el res
    pure Nothing
  Hooks.pure (render {})
  where
  render ctx = do
    HH.span
      [ HP.ref $ RefLabel "badge-svg" ]
      []