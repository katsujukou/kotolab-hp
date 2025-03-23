module Kotolab.HP.Web.View.WorksView where

import Prelude

import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Kotolab.HP.Web.Assets (assets, fromAssetURL)
import Kotolab.HP.Web.Capabilities.MonadAjax (class MonadAjax)
import Kotolab.HP.Web.Component.HTML.PageTitle (pageTitle)
import Kotolab.HP.Web.Component.OpenSourceWork as OSS
import Kotolab.HP.Web.Component.PureScriptBadge as PureScriptBadge
import Type.Proxy (Proxy(..))

-- サーバから取ってきても良いかも
ossWorks :: Array OSS.LibraryInfo
ossWorks =
  [ { name: "Halogen Helix"
    , status: OSS.Stable "v1.0.1"
    , pursVersion: "v0.15.15"
    , home: "https://github.com/katsujukou/purescript-halogen-helix.git"
    , docs: "https://pursuit.purescript.org/packages/purescript-halogen-helix/1.0.1"
    , license: "MIT"
    , description: "PureScript HalogenのSPAにおけるグローバル状態管理のライブラリです。React Reduxに影響を受けています。"
    }
  , { name: "httpurple-aws-lambda"
    , status: OSS.Unstable
    , pursVersion: "v0.15.15"
    , home: "https://github.com/katsujukou/purescript-httpurple-aws-lambda.git"
    , docs: "https://github.com/katsujukou/purescript-httpurple-aws-lambda/blob/main/README.md"
    , license: "MIT"
    , description: "HTTPurpleを使ったREST APIをAWSのLambda関数として動作させるためのアダプタです。雑に作ってるためレジストリにpublishしていません"
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
      , HH.div []
          [ HH.h3 [ HP.class_ $ ClassName "text-lg font-retro font-bold text-pink-700 flex items-center" ]
              [ HH.text "♥ライブラリなど"
              ]
          , HH.div [] $
              ossWorks <#> \libInfo -> do
                HH.slot_ (Proxy :: _ "open-source-work") libInfo.name OSS.make { libInfo }
          ]
      --   , HH.div []
      --       [ HH.h3 [ HP.class_ $ ClassName "text-lg font-retro font-bold text-pink-700" ]
      --           [ HH.text "♥ 勉強会やLT会での発表"
      --           ]
      --       ]
      ]

