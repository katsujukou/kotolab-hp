module Kotolab.HP.Web.Component.OpenSourceWork where

import Prelude

import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Kotolab.HP.Web.Assets (assets, fromAssetURL)

data LibraryStatus = Stable String | Unstable

type LibraryInfo =
  { name :: String
  , status :: LibraryStatus
  , pursVersion :: String
  , home :: String
  , docs :: String
  , license :: String
  , description :: String
  }

type Input =
  { libInfo :: LibraryInfo
  }

make :: forall q o m. H.Component q Input o m
make = Hooks.component \_ { libInfo } -> Hooks.do
  Hooks.pure (render { libInfo })
  where
  render ctx@{ libInfo } = do
    HH.div [ HP.class_ $ ClassName "border border-gray-200 shadow-md my-5 bg-white p-5 " ]
      [ HH.a
          [ HP.class_ $ ClassName "flex items-center mb-2"
          , HP.href libInfo.home
          , HP.target "_blank"
          , HP.rel "noopener"
          ]
          [ HH.div [ HP.class_ $ ClassName "text-lg font-codeblock font-bold text-pink-700" ]
              [ HH.text libInfo.name
              ]
          ]
      , HH.div [ HP.class_ $ ClassName "mt-3" ]
          [ HH.p [ HP.class_ $ ClassName "font-sans text-sm text-gray-700" ]
              [ HH.text libInfo.description
              ]
          ]
      , renderMetaInfo ctx
      ]
  renderMetaInfo { libInfo } = do
    HH.div
      [ HP.class_ $ ClassName "mt-5" ]
      [ HH.div [ HP.class_ $ ClassName "flex flex-wrap items-center" ]
          [
            -- document link
            HH.a
              [ HP.class_ $ ClassName "cursor-pointer flex items-center"
              , HP.href libInfo.docs
              , HP.target "_blank"
              , HP.rel "noopener"
              ]
              [ HH.img
                  [ HP.class_ $ ClassName " w-4 h-4 mx-1 "
                  , HP.src $ fromAssetURL assets.icons.blackCoverBook
                  ]
              ]
          , spacer
          -- license mark
          , HH.div
              [ HP.class_ $ ClassName "flex items-center" ]
              [ HH.img
                  [ HP.class_ $ ClassName "w-4 h-4 mx-1 "
                  , HP.src $ fromAssetURL assets.icons.law
                  ]
              , HH.span [ HP.class_ $ ClassName "text-sm font-josefin-sans font-bold text-red-900" ]
                  [ HH.text libInfo.license ]
              ]
          , spacer
          , -- purescript compiler version
            HH.div [ HP.class_ $ ClassName "flex items-center" ]
              [ HH.img
                  [ HP.class_ $ ClassName "w-4 h-4 mx-1"
                  , HP.src $ fromAssetURL assets.icons.purescript
                  ]
              , HH.span
                  [ HP.class_ $ ClassName "text-sm font-codeblock text-gray-500" ]
                  [ HH.text libInfo.pursVersion ]
              ]
          , spacer
          , -- status (version)
            HH.span
              [ HP.class_ $ ClassName "cursor-pointer flex items-center"
              ]
              [ HH.img
                  [ HP.class_ $ ClassName " w-4 h-4 mx-1 "
                  , HP.src $ fromAssetURL assets.icons.tagGreen
                  ]
              , HH.span [ HP.class_ $ ClassName "font-codeblock text-sm text-green-700 " ]
                  [ HH.text case libInfo.status of
                      Unstable -> "beta"
                      Stable ver -> "stable " <> ver
                  ]
              ]
          ]
      ]

  spacer = do
    HH.span [ HP.class_ $ ClassName "text-gray-500" ] [ HH.text "・" ]
