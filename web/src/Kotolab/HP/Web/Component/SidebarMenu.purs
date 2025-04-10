module Kotolab.HP.Web.Component.SidebarMenu where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Fmt as Fmt
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (captures, useQuery, useState, useTickEffect)
import Halogen.Hooks as Hooks
import Kotolab.HP.Web.Assets (assets, fromAssetURL)
import Kotolab.HP.Web.Component.Types (MenuItem)
import Kotolab.HP.Web.Hooks.UseNavigate (useNavigate)
import Kotolab.HP.Web.Routes (Route)

type Input =
  { value :: Boolean
  , menuItems :: Array MenuItem
  }

data Query a = Toggle a

data Output = CloseClicked | MenuItemClicked Route

make :: forall m. MonadEffect m => H.Component Query Input Output m
make = Hooks.component \{ queryToken, outputToken } inps@{ value } -> Hooks.do
  { currentRoute } <- useNavigate
  display /\ displayId <- useState inps.value

  captures { value } useTickEffect do
    Hooks.put displayId value
    pure Nothing

  useQuery queryToken
    case _ of
      Toggle next -> do
        Console.log "Toggle!"
        Hooks.modify_ displayId not
        pure $ Just next

  let
    handleCloseBtn = do
      Hooks.put displayId false
      Hooks.raise outputToken CloseClicked

    handleMenuItemClick to = do
      Hooks.put displayId false
      Hooks.raise outputToken $ MenuItemClicked to

    ctx =
      { display
      , handleCloseBtn
      , currentRoute: currentRoute
      , menuItems: inps.menuItems
      , handleMenuItemClick
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
        "fixed top-3 right-3 \
        \ flex justify-center items-center \
        \ transition-all duration-400"
    HH.div [ HP.class_ $ ClassName cls ]
      [ HH.div
          [ HP.class_ $ ClassName menuCls ] $
          ctx.menuItems <#> renderMenuItem ctx
      -- 閉じるボタン
      , HH.button
          [ HP.class_ $ ClassName btnCls
          , HE.onClick \_ -> ctx.handleCloseBtn
          ]
          [ HH.span [ HP.class_ $ ClassName "fixed font-josefin-sans text-pink-900 font-light text-xs" ]
              [ HH.text "CLOSE" ]
          , HH.img
              [ HP.src $ fromAssetURL assets.images.roseFrame
              , HP.class_ $ ClassName "top-0 left-0 w-[64px] h-[64px]"
              ]
          ]
      ]

  renderMenuItem ctx menuItem = do
    let
      cls =
        Fmt.fmt
          @"hover:text-blue-500 text-lg my-3 font-josefin-sans \
          \ {activeCls}"
          { activeCls:
              if ctx.currentRoute == Just menuItem.route then "text-pink-700"
              else ""
          }
    HH.div
      [ HP.class_ $ ClassName cls
      , HE.onClick \_ -> ctx.handleMenuItemClick menuItem.route
      ]
      [ HH.text menuItem.label ]
