module Kotolab.HP.Web.App where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Fmt as Fmt
import Halogen (ClassName(..), liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (captures, useState, useTickEffect)
import Halogen.Hooks as Hooks
import Kotolab.HP.Web.Capabilities.MonadAjax (class MonadAjax)
import Kotolab.HP.Web.Component.Footer as Footer
import Kotolab.HP.Web.Component.Header as Header
import Kotolab.HP.Web.Component.HeaderMenu as HeaderMenu
import Kotolab.HP.Web.Component.PageTopButton as PageTopButton
import Kotolab.HP.Web.Component.SidebarMenu as SidebarMenu
import Kotolab.HP.Web.Component.SidebarToggleButton as SidebarToggleButton
import Kotolab.HP.Web.Component.Types (MenuItem)
import Kotolab.HP.Web.Hooks.UseHeight (useHeight)
import Kotolab.HP.Web.Hooks.UseNavigate (useNavigate)
import Kotolab.HP.Web.Routes (Route(..))
import Kotolab.HP.Web.View.ContactView as ContactView
import Kotolab.HP.Web.View.HomeView as HomeView
import Kotolab.HP.Web.View.ProfileView as ProfileView
import Kotolab.HP.Web.View.WorksView as WorksView
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.CSSOMView (ScrollBehavior(..))
import Web.CSSOMView.Element (scrollTo)
import Web.DOM.Element as Element
import Web.HTML as Window

menuItems :: Array MenuItem
menuItems =
  [ { label: "HOME", route: Home }
  , { label: "PROFILE", route: Profile }
  , { label: "WORKS", route: Works }
  , { label: "CONTACT", route: Contact }
  -- , { label: "LINKS", route: Links }
  ]

_mainView :: H.RefLabel
_mainView = H.RefLabel "main-view"

make :: forall q i o m. MonadAjax m => H.Component q i o m
make = Hooks.component \{ slotToken } _ -> Hooks.do
  navigatorApi <- useNavigate
  dispayToggleBtn /\ dispayToggleBtnId <- useState true
  { height } <- useHeight
  shouldDisplayTopButton /\ shouldDisplayTopButtonId <- useState false

  _ <- useMainViewHeight { shouldDisplayTopButtonId } { currentRoute: navigatorApi.currentRoute, height }

  let
    handleSidebarToggleBtn = case _ of
      SidebarToggleButton.Clicked -> do
        Hooks.modify_ dispayToggleBtnId not
        Hooks.tell slotToken (Proxy :: _ "sidebar-menu") unit SidebarMenu.Toggle
      _ -> pure unit

    handleSidebarMenu = case _ of
      SidebarMenu.CloseClicked -> do
        Hooks.put dispayToggleBtnId true

    handleBackToTopClick = case _ of
      PageTopButton.Clicked -> do
        window <- liftEffect $ Window.window
        liftEffect $ scrollTo (Just { behavior: Smooth, top: 10.0, left: 0.0 }) (unsafeCoerce window)

    ctx =
      { dispayToggleBtn
      , handleSidebarToggleBtn
      , handleSidebarMenu
      , currentRoute: navigatorApi.currentRoute
      , shouldDisplayTopButton
      , handleBackToTopClick
      }

  Hooks.pure $ render ctx

  where
  useMainViewHeight nonDeps deps@{ height } = captures deps useTickEffect do
    unless (height == 0) do
      mbMainView <- Hooks.getRef _mainView
      case mbMainView of
        Nothing -> pure unit
        Just mainViewEl -> do
          mainViewHeight <- liftEffect $ Element.scrollHeight mainViewEl
          Hooks.put nonDeps.shouldDisplayTopButtonId (mainViewHeight > Int.toNumber height)

    pure Nothing

  render ctx = do

    HH.div [ HP.class_ $ ClassName "flex flex-col min-h-screen" ]
      [
        -- ヘッダー
        HH.div [ HP.class_ $ ClassName "h-16 flex items-center justify-center bg-pink-200" ]
          [ HH.slot_ (Proxy :: _ "header") unit Header.make {}
          ]
      -- ヘッダーメニュー(PC画面のみ)
      , HH.div [ HP.class_ $ ClassName "hidden sm:block" ]
          [ HH.slot_ (Proxy :: _ "header-menu") unit HeaderMenu.make
              { menuItems }
          ]

      -- メインビュー 
      , HH.div
          [ HP.class_ $ ClassName "flex-1 bg-pink-50 relative scroll-smooth pb-32"
          , HP.ref _mainView
          ]
          [ HH.div
              [ HP.class_ $ ClassName "w-full px-5 sm:w-2/3 lg:w-1/2 sm:mx-auto" ]
              [ renderRouterView ctx

              ]
          , HH.div
              [ HP.class_ $ ClassName $
                  Fmt.fmt
                    @"absolute left-1/2 -translate-x-1/2 bottom-12 sm:bottom-9 \
                    \ {visibility}"
                    { visibility: if ctx.shouldDisplayTopButton then "" else "hidden" }
              ]
              [ HH.slot (Proxy :: _ "back-to-top") unit PageTopButton.make {} ctx.handleBackToTopClick
              ]
          ]

      -- フッター
      , HH.div
          [ HP.class_ $ ClassName "h-16 flex items-center justify-center bg-white text-gray-500 border-t border-t-pink-200 "
          ]
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
      Works -> HH.slot_ (Proxy :: _ "workw-view") unit WorksView.make {}
      Contact -> HH.slot_ (Proxy :: _ "contact-view") unit ContactView.make {}
-- Links -> HH.slot_ (Proxy :: _ "links-view") unit LinksView.make {}
