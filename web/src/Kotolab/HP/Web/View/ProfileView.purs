module Kotolab.HP.Web.View.ProfileView where

import Prelude

import Data.Array (singleton)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Kotolab.HP.Web.Assets (assets, fromAssetURL)
import Kotolab.HP.Web.Component.HTML.PageTitle (pageTitle)

make :: forall q i o m. H.Component q i o m
make = Hooks.component \_ _ -> Hooks.do
  Hooks.pure (render {})
  where
  render _ = do
    let
      bio =
        [ "10月22日生まれ、生まれも育ちも神戸。"
        , "小学生くらいの時に、当時のインターネットで大流行していたハックロム\
          \に触れてしまって以来、コンピューター人間の人生を歩むことに。"
        , "大学卒業後、IT企業への就職を機に、それまで6502アセンブリと\
          \C言語くらいしか知らなかったプログラミングの知識の幅を広げようと\
          \Haskellを勉強したのをきっかけに、関数型の世界にどっぷりハマってしまう。"
        , ""
        , "それ以来プログラミング言語が好きで、最近は型理論など\
          \プログラミング言語の背後にある数理に興味を持ち、日々勉強中。"
        ]
    HH.div
      [ HP.class_ $ ClassName "flex flex-col items-center p-3 "
      ]
      [ pageTitle
          { label: "プロフィール"
          , sub: "Profile"
          }
      , HH.div [ HP.class_ $ ClassName "flex flex-col sm:flex-row sm:w-full" ]
          [ HH.div
              [ HP.class_ $ ClassName "w-80" ]
              [ HH.img
                  [ HP.class_ $ ClassName "w-full h-full object-cover [clip-path:inset(10% round 20px)]"
                  , HP.src $ fromAssetURL assets.images.selfy
                  ]
              ]
          , HH.div [ HP.class_ $ ClassName "flex flex-col justify-center items-center sm:gap-3 flex-1" ]
              [ HH.div
                  [ HP.class_ $ ClassName "text-[1.7rem] mt-3 sm:mt-0"
                  ]
                  [ HH.text "こと" ]
              , HH.div
                  [ HP.class_ $ ClassName "text-[1.25rem] text-gray-500"
                  ]
                  [ HH.text "Koto"
                  ]
              , HH.div
                  [ HP.class_ $ ClassName "text-gray-700 text-sm px-5" ] $
                  (map (HH.p [ HP.class_ $ ClassName "my-3" ] <<< singleton <<< HH.text) bio)
              ]
          ]
      , HH.div [ HP.class_ $ ClassName "bg-white w-full p-5 pb-0 my-5 " ]
          [ HH.h2 [ HP.class_ $ ClassName "text-center text-pink-500 mb-3" ]
              [ HH.text "ことメモ"
              ]
          , renderProfileTable
          ]

      , HH.div
          [ HP.class_ $ ClassName "flex flex-col items-center"
          ]
          [ HH.h2 []
              [ HH.text "主な活動"
              ]
          , HH.div [ HP.class_ $ ClassName "my-2" ]
              [ HH.h3 [ HP.class_ $ ClassName "text-center" ]
                  [ HH.text "λ Kansaiを主催"
                  ]
              , HH.p []
                  [ HH.text "関数型好きが高じて, "
                  , HH.a
                      [ HP.class_ $ ClassName "text-indigo-500 hover:text-indigo-900 hover:underline"
                      , HP.href "https://lambda-kansai.org"
                      , HP.target "_blank"
                      , HP.rel "noopener"
                      ]
                      [ HH.text "関数型プログラミング愛好家のためのコミュニティ" ]
                  , HH.text "を自らつくりました。"
                  ]
              ]
          , HH.div [ HP.class_ $ ClassName "my-2" ]
              [ HH.h3
                  [ HP.class_ $ ClassName "text-center" ]
                  [ HH.text "バーテンダーとして" ]
              , HH.p []
                  [ HH.text "神戸三宮のエンジニアが集まるバー "
                  , HH.a
                      [ HP.class_ $ ClassName "text-indigo-500 hover:text-indigo-900 hover:underline"
                      , HP.href "https://hackbar.jp/"
                      , HP.target "_blank"
                      , HP.rel "noopener"
                      ]
                      [ HH.text "HACK.BAR" ]
                  , HH.text " でバーテンダーをしています🍸️"
                  , HH.br_
                  , HH.text "ぜひあそびにきてください♪"
                  ]
              ]
          ]
      ]

  renderProfileTable = do
    let
      items =
        [ { item: "すきなもの", desc: "可愛い服／コスメ／PureScript／数学とか計算機科学のテキスト／長唄" }
        , { item: "推　し", desc: "弓木奈於ちゃん(乃木坂46)／谷崎早耶ちゃん(≠ME)／幕末志士(ゲーム実況者)／さくらちゃん" }
        , { item: "すきな食べ物", desc: "お好み焼き／お芋のスイーツ／ボンタンアメ" }
        , { item: "特　技", desc: "長唄三味線（杵勝派 名取芸人）" }
        , { item: "休日のすごし方", desc: "ショッピング／コンカフェ行く" }
        , { item: "すきなブランド", desc: "AnkRouge／ミシェルマカロン／dazzlin／Cherimi" }
        , { item: "すきな作品", desc: "ねこきっさ／ぱにぽに／AIR／ゆるゆり／大室家／やがて君になる／桜Trick" }
        ]
      renderItem item = do
        HH.div
          [ HP.class_ $ ClassName "pb-3 " ]
          [ HH.div
              [ HP.class_ $ ClassName "flex text-sm/6 font-medium text-gray-900" ]
              [ HH.div [ HP.class_ $ ClassName "mr-1 text-gray-500" ]
                  [ HH.text "♥"
                  , HH.text item.item
                  , HH.text "♥"
                  ]
              , HH.div [ HP.class_ $ ClassName "flex-1 my-auto" ]
                  [ HH.hr [ HP.class_ $ ClassName "border-dashed border-gray-600" ]
                  ]
              ]
          , HH.div [ HP.class_ $ ClassName "mt-1 text-xs/6 text-pink-500 sm:col-span-2 sm:mt-0" ]
              [ HH.text item.desc
              ]
          ]
    HH.dl [ HP.class_ $ ClassName "" ]
      $ items <#> renderItem

