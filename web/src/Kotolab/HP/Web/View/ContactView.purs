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
import Kotolab.HP.Web.Hooks.UseApp (AppMode(..), useApp)
import Promise.Aff (toAffE)
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
make = Hooks.component \_ _ -> Hooks.do
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

      , HH.div [ HP.class_ $ ClassName "flex justify-center items-center m-1 " ] $ fold
          [ [ HH.button
                [ HP.class_ $ ClassName "m-1 "
                , HE.onClick \_ -> ctx.handleEmailIconClick
                ]
                [ HH.img
                    [ HP.class_ $ ClassName "h-8 w-8"
                    , HP.src $ fromAssetURL assets.icons.iconEmail
                    ]
                ]
            ]
          , externalLinks <#> renderExternalLink ctx
          ]
      ]

  renderExternalLink _ item = do
    HH.a
      [ HP.class_ $ ClassName "m-1 "
      , HP.href item.href
      ]
      [ HH.img
          [ HP.class_ $ ClassName "h-8 w-8"
          , HP.src $ fromAssetURL item.icon
          ]
      ]
