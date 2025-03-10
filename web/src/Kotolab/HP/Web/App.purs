module Kotolab.HP.Web.App where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (useState)
import Halogen.Hooks as Hooks
import Kotolab.HP.Web.Component.Footer as Footer
import Kotolab.HP.Web.Component.Header as Header
import Kotolab.HP.Web.Component.SidebarMenu as SidebarMenu
import Kotolab.HP.Web.Component.SidebarToggleButton as SidebarToggleButton
import Kotolab.HP.Web.Component.Types (MenuItem)
import Kotolab.HP.Web.Hooks.UseApp (useApp)
import Kotolab.HP.Web.Routes (Route(..))
import Kotolab.HP.Web.View.ContactView as ContactView
import Kotolab.HP.Web.View.HomeView as HomeView
import Kotolab.HP.Web.View.LinksView as LinksView
import Kotolab.HP.Web.View.ProfileView as ProfileView
import Type.Proxy (Proxy(..))

menuItems :: Array MenuItem
menuItems =
  [ { label: "HOME", route: Home }
  , { label: "PROFILE", route: Profile }
  , { label: "CONTACT", route: Contact }
  , { label: "LINKS", route: Links }
  ]

make :: forall q i o m. MonadAff m => H.Component q i o m
make = Hooks.component \{ slotToken } _ -> Hooks.do
  appApi <- useApp
  dispayToggleBtn /\ dispayToggleBtnId <- useState true
  let
    handleSidebarToggleBtn = case _ of
      SidebarToggleButton.Clicked -> do
        Hooks.modify_ dispayToggleBtnId not
        Hooks.tell slotToken (Proxy :: _ "sidebar-menu") unit SidebarMenu.Toggle
      _ -> pure unit

    handleSidebarMenu = case _ of
      SidebarMenu.CloseClicked -> do
        Hooks.put dispayToggleBtnId true

    ctx =
      { dispayToggleBtn
      , handleSidebarToggleBtn
      , handleSidebarMenu
      , currentRoute: appApi.currentRoute
      }

  Hooks.pure $ render ctx

  where
  render ctx = do

    HH.div [ HP.class_ $ ClassName "flex flex-col min-h-screen" ]
      [
        -- ヘッダー
        HH.div [ HP.class_ $ ClassName "h-16 flex items-center justify-center bg-pink-200" ]
          [ HH.slot_ (Proxy :: _ "header") unit Header.make {}
          ]

      -- メインビュー 
      , HH.div [ HP.class_ $ ClassName "flex-1 px-5 bg-pink-50" ]
          [ renderRouterView ctx
          ]

      -- フッター
      , HH.div [ HP.class_ $ ClassName "h-16 flex items-center justify-center bg-white text-gray-500" ]
          [ HH.slot_ (Proxy :: _ "footer") unit Footer.make {}
          ]

      -- モバイル時のみ、サイドバーにメニューを表示
      , HH.div
          [ HP.class_ $ ClassName "block sm:hidden" ]
          [ HH.slot (Proxy :: _ "sidebar-menu") unit SidebarMenu.make
              { value: false
              , menuItems
              }
              ctx.handleSidebarMenu
          -- サイドバー切り替えボタン
          , HH.slot (Proxy :: _ "sidebar-toggle-btn") unit SidebarToggleButton.make { value: ctx.dispayToggleBtn }
              ctx.handleSidebarToggleBtn
          ]
      ]

  renderRouterView ctx = case ctx.currentRoute of
    Nothing -> HH.text "Page Not Found"
    Just r -> case r of
      Home -> HH.slot_ (Proxy :: _ "make-view") unit HomeView.make {}
      Profile -> HH.slot_ (Proxy :: _ "profile-view") unit ProfileView.make {}
      Contact -> HH.slot_ (Proxy :: _ "contact-view") unit ContactView.make {}
      Links -> HH.slot_ (Proxy :: _ "links-view") unit LinksView.make {}
