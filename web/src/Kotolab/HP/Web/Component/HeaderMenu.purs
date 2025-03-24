module Kotolab.HP.Web.Component.HeaderMenu where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Fmt as Fmt
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Kotolab.HP.Web.Component.Types (MenuItem)
import Kotolab.HP.Web.Hooks.UseApp (useApp)

type Input =
  { menuItems :: Array MenuItem
  }

make :: forall q o m. MonadEffect m => H.Component q Input o m
make = Hooks.component \_ inps -> Hooks.do
  appApi <- useApp
  let
    handleMenuItem menuItem = do
      appApi.navigateTo menuItem.route

    ctx =
      { menuItems: inps.menuItems
      , currentRoute: appApi.currentRoute
      , handleMenuItem
      }

  Hooks.pure (render ctx)
  where
  render ctx = do
    HH.div
      [ HP.class_ $ ClassName "w-full flex items-center justify-center gap-7 p-4" ] $
      ctx.menuItems <#> \menuItem -> do
        let
          menuItemCls = Fmt.fmt
            @"font-josefin-sans text-lg cursor-pointer {active}"
            { active: if Just menuItem.route == ctx.currentRoute then "text-pink-700 " else "" }
        HH.div
          [ HP.class_ $ ClassName menuItemCls
          , HE.onClick \_ -> ctx.handleMenuItem menuItem
          ]
          [ HH.text menuItem.label
          ]