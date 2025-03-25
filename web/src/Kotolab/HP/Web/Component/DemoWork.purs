module Kotolab.HP.Web.Component.DemoWork where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Kotolab.HP.Web.Assets (assets, fromAssetURL)

type DemoInfo =
  { name :: String
  , home :: Maybe String
  , repo :: String
  , license :: String
  , pursVersion :: String
  , description :: String
  }

type Input =
  { demoInfo :: DemoInfo
  }

make :: forall q o m. H.Component q Input o m
make = Hooks.component \_ { demoInfo } -> Hooks.do
  Hooks.pure (render { demoInfo })
  where
  render ctx@{ demoInfo } = do
    HH.div [ HP.class_ $ ClassName "max-w-128 border border-gray-200 shadow-md  bg-white p-5 " ]
      [ HH.div [ HP.class_ $ ClassName "flex items-center" ]
          [ HH.div [ HP.class_ $ ClassName "text-lg font-codeblock font-bold text-pink-700" ]
              [ HH.text demoInfo.name
              ]
          , HH.div [ HP.class_ $ ClassName "ml-auto" ]
              [ HH.div [ HP.class_ $ ClassName "flex flex-row-reverse gap-3" ]
                  [ case demoInfo.home of
                      Just homeLink -> do
                        -- homepage link
                        HH.a
                          [ HP.class_ $ ClassName "cursor-pointer flex items-center"
                          , HP.href homeLink
                          , HP.target "_blank"
                          , HP.rel "noopener"
                          ]
                          [ HH.img
                              [ HP.class_ $ ClassName " w-4 h-4"
                              , HP.src $ fromAssetURL assets.icons.homeBlue
                              ]
                          ]
                      Nothing -> HH.text ""
                  ]

              ]
          ]
      , HH.div [ HP.class_ $ ClassName "mt-3" ]
          [ HH.p [ HP.class_ $ ClassName "font-sans text-sm text-gray-700" ]
              [ HH.text demoInfo.description
              ]
          ]
      , renderMetaInfo ctx
      ]
  renderMetaInfo { demoInfo } = do
    HH.div
      [ HP.class_ $ ClassName "mt-5 text-xs" ]
      [ HH.div [ HP.class_ $ ClassName "flex flex-wrap items-center" ] $
          [ -- repo link
            HH.a
              [ HP.class_ $ ClassName "cursor-pointer flex items-center"
              , HP.href demoInfo.repo
              , HP.target "_blank"
              , HP.rel "noopener"
              ]
              [ HH.img
                  [ HP.class_ $ ClassName " w-4 h-4"
                  , HP.src $ fromAssetURL assets.icons.githubMark
                  ]
              ]
          , spacer
          -- license mark
          , HH.div
              [ HP.class_ $ ClassName "flex items-center" ]
              [ HH.img
                  [ HP.class_ $ ClassName "w-3 h-3 m-1 "
                  , HP.src $ fromAssetURL assets.icons.law
                  ]
              , HH.span [ HP.class_ $ ClassName "font-josefin-sans font-bold text-red-900" ]
                  [ HH.text demoInfo.license ]
              ]
          , spacer
          , -- purescript compiler version
            HH.div
              [ HP.class_ $ ClassName "flex items-center" ]
              [ HH.img
                  [ HP.class_ $ ClassName "w-3 h-3 m-1"
                  , HP.src $ fromAssetURL assets.icons.purescript
                  ]
              , HH.span
                  [ HP.class_ $ ClassName "font-codeblock text-gray-500" ]
                  [ HH.text demoInfo.pursVersion ]
              ]
          ]
      ]

  spacer = do
    HH.span [ HP.class_ $ ClassName "text-gray-500" ] [ HH.text "・" ]
