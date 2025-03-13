module Kotolab.HP.Web.Component.SidebarMenu where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Fmt as Fmt
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (useQuery, useState)
import Halogen.Hooks as Hooks
import Kotolab.HP.Web.Assets (assets, fromAssetURL)
import Kotolab.HP.Web.Component.Types (MenuItem)
import Kotolab.HP.Web.Hooks.UseApp (useApp)

type Input =
  { value :: Boolean
  , menuItems :: Array MenuItem
  }

data Query a = Toggle a

data Output = CloseClicked

make :: forall m. MonadEffect m => H.Component Query Input Output m
make = Hooks.component \{ queryToken, outputToken } inps -> Hooks.do
  appApi <- useApp
  display /\ displayId <- useState inps.value

  useQuery queryToken
    case _ of
      Toggle next -> do
        Hooks.modify_ displayId not
        pure $ Just next

  let
    handleCloseBtn = do
      Hooks.put displayId false
      Hooks.raise outputToken CloseClicked

    handleNavigate to = do
      appApi.navigateTo to
      handleCloseBtn

    ctx =
      { display
      , handleCloseBtn
      , currentRoute: appApi.currentRoute
      , menuItems: inps.menuItems
      , handleNavigate
      }

  Hooks.pure $ render ctx
  where
  render ctx = do
    let
      cls =
        Fmt.fmt
          @"fixed top-0 right-0 w-3/4 h-full bg-white shadow-lg \
          \ border border-0 border-l-gray-500 p-5 \
          \ transform transition-transform duration-300 translate-x-{transl_x}"
          { transl_x: if ctx.display then "0" else "full" }

      menuCls =
        "h-full flex flex-col justify-center items-center"

      btnCls =
        "fixed top-3 right-3 bg-pink-300 w-[40px] h-[40px] rounded-lg \
        \ flex justify-center items-center active:bg-pink-400 "

    HH.div [ HP.class_ $ ClassName cls ]
      [ HH.div
          [ HP.class_ $ ClassName menuCls ] $
          ctx.menuItems <#> renderMenuItem ctx
      -- 閉じるボタン
      , HH.button
          [ HP.class_ $ ClassName btnCls
          , HE.onClick \_ -> ctx.handleCloseBtn
          ]
          [ HH.img
              [ HP.src $ fromAssetURL assets.icons.close
              , HP.class_ $ ClassName "w-[32px] h-[32px]"
              ]
          ]
      ]

  renderMenuItem ctx menuItem = do
    let
      cls =
        Fmt.fmt
          @"hover:text-blue-500 text-lg my-3 font-genei \
          \ {activeCls}"
          { activeCls:
              if ctx.currentRoute == Just menuItem.route then "text-pink-400"
              else ""
          }
    HH.div
      [ HP.class_ $ ClassName cls
      , HE.onClick \_ -> ctx.handleNavigate menuItem.route
      ]
      [ HH.text menuItem.label ]
