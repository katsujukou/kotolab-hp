module Kotolab.HP.Web.View.HomeView where

import Prelude

import Data.Codec.Argonaut as CA
import Data.Either (either)
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Kotolab.HP.API.Schema.Json as Json
import Kotolab.HP.Web.Component.HTML.PageTitle (pageTitle)
import Kotolab.HP.Web.Component.HackbarAttendInfo as HackbarAttendInfo
import Kotolab.HP.Web.Component.Types (HackbarAttendInfo, hackbarAttendInfo)
import Type.Proxy (Proxy(..))

attendList :: Array HackbarAttendInfo
attendList = either (const []) identity $ Json.parse (CA.array hackbarAttendInfo)
  """
  [
    {
      "date": "2025-04-05",
      "startTime": [18,0],
      "endTime": [23,0]
    },
    {
      "date": "2025-04-18",
      "startTime": [19,30],
      "endTime": [23,0]
    },
    {
      "date": "2025-04-25",
      "startTime": [19,30],
      "endTime": [23,0]
    },
    {
      "date": "2025-04-30",
      "startTime": [18,0],
      "endTime": [23,00]
    }
  ]
  """

make :: forall q i o m. MonadEffect m => H.Component q i o m
make = Hooks.component \_ _ -> Hooks.do
  Hooks.pure (render {})
  where
  render _ = do
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
              { attendList }
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