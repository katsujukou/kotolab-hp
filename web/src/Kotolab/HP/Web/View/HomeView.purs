module Kotolab.HP.Web.View.HomeView where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (useLifecycleEffect)
import Halogen.Hooks as Hooks
import Kotolab.HP.Web.Capabilities.MonadAjax (class MonadAjax)
import Kotolab.HP.Web.Component.HTML.PageTitle (pageTitle)
import Kotolab.HP.Web.Component.HackbarAttendInfo as HackbarAttendInfo
import Kotolab.HP.Web.Hooks.UseHackbarAttendInfoList (useHackbarAttendInfoList)
import Type.Proxy (Proxy(..))

make
  :: forall q i o m env
   . MonadAsk { apiBaseURL :: String | env } m
  => MonadAjax m
  => H.Component q i o m
make = Hooks.component \_ _ -> Hooks.do
  hackbarAttendInfoApi <- useHackbarAttendInfoList

  useLifecycleEffect do
    hackbarAttendInfoApi.reload
    pure Nothing

  let
    ctx =
      { hackbarAttendInfoList: hackbarAttendInfoApi.hackbarAttendInfoList
      }
  Hooks.pure (render ctx)
  where
  render ctx = do
    HH.div
      [ HP.class_ $ ClassName "flex flex-col items-center "
      ]
      [ HH.div [ HP.class_ $ ClassName "mt-9" ]
          [ pageTitle
              { sub: "Koto's Home"
              , label: "ことさんちへ　ようこそ！"
              }
          ]
      , HH.div [ HP.class_ $ ClassName "m-4 flex flex-col items-center" ]
          [ HH.h3 [ HP.class_ $ ClassName "font-yomogi text-lg text-pink-700 m-4" ]
              [ HH.text "─ インフォメーション ─ " ]
          , HH.div [ HP.class_ $ ClassName "text-gray-700 mb-4" ]
              [ HH.text " 現在、 新しいお知らせはありません。" ]
          ]
      , HH.div [ HP.class_ $ ClassName "my-4 " ]
          [ HH.slot_ (Proxy :: _ "hackbar-attend-info") unit HackbarAttendInfo.make
              { attendList: ctx.hackbarAttendInfoList }
          ]
      , HH.div [ HP.class_ $ ClassName "flex flex-col items-center" ]
          [ HH.h2 [ HP.class_ $ ClassName "font-yomogi text-lg text-pink-700 m-4" ]
              [ HH.text "─ すぺしゃる さんくす ─ " ]
          , HH.p [ HP.class_ $ ClassName "sm:w-1/2 w-[80%] font-josefin-sans text-sm text-gray-500" ]
              [ HH.text "このWebサイトは、"
              , HH.a
                  [ HP.class_ $ ClassName "text-indigo-500 hover:underline"
                  , HP.href "https://github.com/purescript-halogen/purescript-halogen"
                  , HP.target "_blank"
                  , HP.rel "noopener"
                  ]
                  [ HH.text "PureScript Halogen" ]
              , HH.text "で実装されており、"
              , HH.a
                  [ HP.class_ $ ClassName "text-indigo-500 hover:underline"
                  , HP.href "https://www.cloudflare.com/"
                  , HP.target "_blank"
                  , HP.rel "noopener"
                  ]
                  [ HH.text "Cloudflare Pages" ]
              , HH.text "にデプロイされ公開されています。"
              , HH.text "素晴らしいプロジェクトを公開してくださっている偉大なるデベロッパー達に、多大な感謝と尊敬の念をここに表します。"
              ]
          ]
      ]