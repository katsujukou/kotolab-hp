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
        , ""
        , "小さい時からレトロゲームが好きで、ずっとファミコンばかりやってた。スーパーマリオブラザーズ２はわたしの青春。"
        , "はじめてPCが家に来た頃、兄の影響で当時インターネット上で流行していた改造マリオの世界に触れたのがきっかけとなり、\
          \プログラミングの世界に足を踏み入れる。"
        , ""
        , "大学卒業後、IT企業への就職を機に、それまで6502アセンブリと\
          \C言語くらいしか知らなかったプログラミングの知識の幅を広げようと\
          \Haskellを勉強したのをきっかけに、関数型の世界にどっぷりハマってしまう。"
        , ""
        , "プログラミング言語が好きで、最近はコンパイラの設計や型理論など\
          \プログラミング言語の背後にある数理に興味を持ち、日々勉強中です。"
        ]
    HH.div
      [ HP.class_ $ ClassName "flex flex-col items-center p-4"
      ]
      [ pageTitle
          { label: "じこしょうかい"
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
                  [ HP.class_ $ ClassName "text-[1.7rem] mt-3 sm:mt-0 font-retro"
                  ]
                  [ HH.text "こと" ]
              , HH.div
                  [ HP.class_ $ ClassName "text-[1.25rem] text-gray-500 font-genei"
                  ]
                  [ HH.text "Koto"
                  ]
              , HH.div
                  [ HP.class_ $ ClassName "text-gray-700 text-sm px-5 font-josefin-sans" ] $
                  (map (HH.p [ HP.class_ $ ClassName "my-3" ] <<< singleton <<< HH.text) bio)
              ]
          ]
      , HH.div [ HP.class_ $ ClassName "bg-white w-full p-5 sm:p-9 pb-0 my-5 " ]
          [ renderProfileTable
          ]

      , HH.div
          [ HP.class_ $ ClassName "w-full flex flex-col items-center"
          ]
          [ HH.h2
              [ HP.class_ $ ClassName "w-full py-3 flex justify-center items-center border border-dashed border-gray-400 border-l-0 border-r-0 mt-5 mb-4" ]
              [ HH.span
                  [ HP.class_ $ ClassName "font-retro text-[1.5rem] font-bold " ]
                  [ HH.text "＊ 主な活動 ＊" ]
              ]

          , HH.div
              [ HP.class_ $ ClassName "grid sm:grid-cols-2 sm:gap-3" ]
              [ renderActivity
                  "関数型プログラミング愛好家として"
                  [ HH.text "PureScriptをこよなく愛し、ブログの執筆やライブラリ作成などで日々布教活動中。"
                  , HH.br_
                  , HH.text "当然このHPもPureScriptでできてます♥"
                  ]

              , renderActivity "コミュニティオーナーとして"
                  [ HH.text "関数型好きが高じて、関数型プログラミング愛好家のためのコミュニティ"
                  , HH.a
                      [ HP.class_ $ ClassName "text-indigo-500 hover:text-indigo-900 hover:underline"
                      , HP.href "https://lambda-kansai.org"
                      , HP.target "_blank"
                      , HP.rel "noopener"
                      ]
                      [ HH.text "λ Kansai" ]
                  , HH.text "を自ら立ち上げました"
                  ]

              , renderActivity
                  "バーテンダーとして"
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
        [ { item: "すきなもの", desc: "可愛いお洋服／コスメとか香水／PureScript／数学とか計算機科学のテキスト／長唄" }
        , { item: "推　し", desc: "弓木奈於ちゃん(乃木坂46)／谷崎早耶ちゃん(≠ME)／幕末志士(ゲーム実況者)／さくらちゃん" }
        , { item: "すきな食べ物", desc: "お好み焼き／CoCo壱カレー／お芋のスイーツ／ボンタンアメ" }
        , { item: "特　技", desc: "長唄三味線（杵勝派 名取芸人）" }
        , { item: "休日のすごし方", desc: "ショッピング／コンカフェ行く" }
        , { item: "すきなブランド", desc: "AnkRouge／EATME／michellMacaron／Cherimi" }
        -- , { item: "すきな作品", desc: "ねこきっさ／ぱにぽに／AIR／ゆるゆり／大室家／やがて君になる／桜Trick／MOTHER／星のカービィシリーズ（特に夢の泉の物語）" }
        ]
      renderItem item = do
        HH.div
          [ HP.class_ $ ClassName "pb-3 " ]
          [ HH.div
              [ HP.class_ $ ClassName "flex text-sm/6 sm:text-lg/12 font-medium text-gray-900 font-yomogi" ]
              [ HH.div [ HP.class_ $ ClassName "mr-1 text-gray-500" ]
                  [ HH.text "♥"
                  , HH.text item.item
                  , HH.text "♥"
                  ]
              , HH.div [ HP.class_ $ ClassName "flex-1 my-auto" ]
                  [ HH.hr [ HP.class_ $ ClassName "border-dashed border-gray-600" ]
                  ]
              ]
          , HH.div [ HP.class_ $ ClassName "mt-1 text-xs/6 sm:text-base  text-pink-500 sm:col-span-2 sm:mt-0 font-yomogi" ]
              [ HH.text item.desc
              ]
          ]
    HH.div []
      [ HH.h2
          [ HP.class_ $ ClassName "text-center text-pink-500 mb-3 font-retro font-bold text-lg sm:text-xl" ]
          [ HH.text "✏️ ことメモ 🎀" ]

      , HH.dl
          [ HP.class_ $ ClassName "" ] $
          items <#> renderItem
      ]

  renderActivity title contentHtmls = do
    HH.div [ HP.class_ $ ClassName "m-2 sm:m-4 w-80 sm:col-span-1" ]
      [ HH.h3
          [ HP.class_ $ ClassName "my-4 text-center font-retro text-[1.2rem] font-bold sm:my-2 text-pink-700" ]
          [ HH.text title ]
      , HH.p [ HP.class_ $ ClassName "text-gray-700 font-josefin-sans" ]
          contentHtmls
      ]

