module Kotolab.HP.Web.View.ContactView where

import Prelude

import Data.Array (fold)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..), liftAff, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Kotolab.HP.Web.Assets (AssetURL, assets, fromAssetURL)
import Kotolab.HP.Web.Component.HTML.PageTitle (pageTitle)
import Kotolab.HP.Web.Component.ToastPopUp as ToastPopUp
import Kotolab.HP.Web.Hooks.UseApp (AppMode(..), useApp)
import Promise.Aff (toAffE)
import Type.Proxy (Proxy(..))
import Web.Clipboard as CB
import Web.HTML as HTML
import Web.HTML.Location (setHref)
import Web.HTML.Window as Window

type ExternalLink =
  { label :: Maybe String
  , icon :: AssetURL
  , href :: String
  }

externalLinks :: Array ExternalLink
externalLinks =
  [ { label: Nothing
    , icon: assets.icons.twitterLogo
    , href: "https://twitter.com/kotolabdev"
    }
  , { label: Nothing
    , icon: assets.icons.githubMark
    , href: "https://github.com/katsujukou"
    }
  ]

make :: forall q i o m. MonadAff m => H.Component q i o m
make = Hooks.component \{ slotToken } _ -> Hooks.do
  appApi <- useApp
  let
    handleEmailIconClick = do
      let
        email = "kotolabdev@gmail.com"
      appApi.getMode >>= case _ of
        SmartPhone -> do
          liftEffect $ HTML.window >>= Window.location >>= setHref ("mailto:" <> email)

        PC -> do
          mbCb <- liftEffect $ HTML.window >>= Window.navigator >>= CB.clipboard
          case mbCb of
            Nothing -> pure unit
            Just cb -> do
              liftAff $ toAffE $ CB.writeText email cb
              Hooks.tell slotToken (Proxy :: _ "email-btn-toast") unit ToastPopUp.Display

    ctx =
      { handleEmailIconClick
      }
  Hooks.pure (render ctx)
  where
  render ctx = do
    HH.div [ HP.class_ $ ClassName "flex flex-col items-center p-4" ]
      [ pageTitle
          { label: "おといあわせ"
          , sub: "Contact"
          }

      , HH.div [ HP.class_ $ ClassName "flex justify-center items-center m-1 gap-3" ] $ fold
          [ [ HH.button
                [ HP.class_ $ ClassName "m-1 relative cursor-pointer"
                , HE.onClick \_ -> ctx.handleEmailIconClick
                ]
                [ HH.img
                    [ HP.class_ $ ClassName "h-8 w-8"
                    , HP.src $ fromAssetURL assets.icons.iconEmail
                    ]
                , HH.slot_ (Proxy :: _ "email-btn-toast") unit ToastPopUp.make {}
                ]
            ]
          , externalLinks <#> renderExternalLink ctx
          ]
      , HH.div [ HP.class_ $ ClassName "my-5" ]
          [ HH.p
              [ HP.class_ $ ClassName "font-josefin-sans text-pink-700" ]
              [ HH.text "ことへのご連絡（お仕事の相談や、勉強会やLT会での登壇の依頼など）は、メール・TwitterのDM等をご利用ください。"
              ]
          , HH.ul [ HP.class_ $ ClassName "my-3 text-gray-700" ]
              $ renderRemarks
          ]
      ]

  renderRemarks = do
    let
      remarks =
        [ "当方、社不につきなかなか返信が返ってこないかもしれませんがご容赦ください"
        , "HACK.BAR出勤時に直接お店に来ていただくのが一番手っ取り早く確実です"
        , "当サイトはリンクフリーです"
        ]
    remarks <#> \remark -> do
      HH.li [ HP.class_ $ ClassName "flex font-yomogi gap-3 mb-3" ]
        [ HH.span [ HP.class_ $ ClassName "no-selection" ]
            [ HH.text "＊" ]
        , HH.p []
            [ HH.text remark ]
        ]

  renderExternalLink _ item = do
    HH.a
      [ HP.class_ $ ClassName "h-8 w-8"
      , HP.href item.href
      , HP.target "_blank"
      , HP.rel "noopener"
      ]
      [ HH.img
          [ HP.class_ $ ClassName ""
          , HP.src $ fromAssetURL item.icon
          ]
      ]
