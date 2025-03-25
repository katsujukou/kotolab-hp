module Kotolab.HP.Web.View.WorksView where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Kotolab.HP.Web.Capabilities.MonadAjax (class MonadAjax)
import Kotolab.HP.Web.Component.DemoWork as Demo
import Kotolab.HP.Web.Component.HTML.PageTitle (pageTitle)
import Kotolab.HP.Web.Component.OpenSourceWork as OSS
import Type.Proxy (Proxy(..))

-- サーバから取ってきても良いかも
ossWorks :: Array OSS.LibraryInfo
ossWorks =
  [ { name: "Halogen Helix"
    , status: OSS.Stable "v1.0.1"
    , type: OSS.PureScriptLibrary "v0.15.15"
    , home: "https://github.com/katsujukou/purescript-halogen-helix.git"
    , docs: Just "https://pursuit.purescript.org/packages/purescript-halogen-helix/1.0.1"
    , license: "MIT"
    , description: "PureScript HalogenのSPAにおけるグローバル状態管理のライブラリです。React Reduxに影響を受けています。"
    }
  , { name: "httpurple-aws-lambda"
    , status: OSS.Unstable
    , type: OSS.PureScriptLibrary "v0.15.15"
    , home: "https://github.com/katsujukou/purescript-httpurple-aws-lambda.git"
    , docs: Just "https://github.com/katsujukou/purescript-httpurple-aws-lambda/blob/main/README.md"
    , license: "MIT"
    , description: "HTTPurpleを使ったREST APIをAWSのLambda関数として動作させるためのアダプタです。雑に作ってるためレジストリにpublishしていません"
    }
  , { name: "Nesasm Nix"
    , status: OSS.Unstable
    , type: OSS.Nix
    , home: "https://github.com/katsujukou/nesasm.nix"
    , docs: Nothing
    , license: "MIT"
    , description:
        "NESASMをNixでインストールできるようにする、誠に誰得なやつ。長らく放置しているが、それ以上にnesasm自体がアプデされていないためご容赦。\
        \せめてflakeには対応したいけどなあ..."
    }
  ]

demoWorks :: Array Demo.DemoInfo
demoWorks =
  [ { name: "TAPL in PureScript"
    , description: "『Types And Programming Languages』を読むにあたり、本の中で作られている型チェッカー/インタープリタをPureScriptで実装したもの。"
    , home: Nothing
    , repo: "https://github.com/katsujukou/TaPL-in-PureScript.git"
    , license: "MIT"
    , pursVersion: "v0.15.x"
    }
  , { name: "DAM4G"
    , description:
        """GDGoC Osakaの「低レイヤー講演会」で登壇させて頂いた際の発表「関数型言語の実行モデル」の中で、デモンストレーションとして作った自作言語のWebベース実行環境です。
ただのデモ用でありながら、それなりに作り込んでいて、ADTを定義してパターンマッチしたりとかはできます。
実行モデルとしては、在りし日のOCamlであるZINCのバイトコードによく似たものにコンパイルされます。"""

    , home: Just "https://katsujukou.github.io/dam4g/"
    , repo: "https://github.com/katsujukou/dam4g.git"
    , license: "MIT"
    , pursVersion: "v0.15.15"
    }
  ]

make :: forall q i o m. MonadAjax m => H.Component q i o m
make = Hooks.component \_ _ -> Hooks.do
  Hooks.pure (render {})
  where
  render _ = do
    HH.div
      [ HP.class_ $ ClassName "flex flex-col items-center p-4"
      ]
      [ pageTitle
          { label: "つくったもの"
          , sub: "Works"
          }
      , HH.div [ HP.class_ $ ClassName "my-5 flex flex-col items-center" ]
          [ HH.h3 [ HP.class_ $ ClassName "text-xl font-retro font-bold text-pink-700 flex items-center my-5" ]
              [ HH.text "♥ライブラリなど"
              ]
          , HH.div [ HP.class_ $ ClassName "flex justify-center flex-wrap gap-7" ] $
              ossWorks <#> \libInfo -> do
                HH.slot_ (Proxy :: _ "open-source-work") libInfo.name OSS.make { libInfo }
          ]
      , HH.div [ HP.class_ $ ClassName "my-5 flex flex-col items-center" ]
          [ HH.h3 [ HP.class_ $ ClassName "text-xl font-retro font-bold text-pink-700 flex items-center my-5 flex items-start" ]
              [ HH.span_ [ HH.text "♥" ]
              , HH.span_ [ HH.text "デモンストレーション目的のプロジェクト" ]
              ]
          , HH.div [ HP.class_ $ ClassName "flex justify-center flex-wrap gap-7" ] $
              demoWorks <#> \demoInfo -> do
                HH.slot_ (Proxy :: _ "demo-work") demoInfo.name Demo.make { demoInfo }
          ]
      --   , HH.div []
      --       [ HH.h3 [ HP.class_ $ ClassName "text-lg font-retro font-bold text-pink-700" ]
      --           [ HH.text "♥ 勉強会やLT会での発表"
      --           ]
      --       ]
      ]

