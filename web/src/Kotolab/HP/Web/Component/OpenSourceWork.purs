module Kotolab.HP.Web.Component.OpenSourceWork where

import Prelude

import Data.Array (fold)
import Data.Maybe (Maybe(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Kotolab.HP.Web.Assets (assets, fromAssetURL)

data OSSType = PureScriptLibrary String | Nix | Misc

data LibraryStatus = Stable String | Unstable

type LibraryInfo =
  { name :: String
  , status :: LibraryStatus
  , type :: OSSType
  , home :: String
  , docs :: Maybe String
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
    HH.div [ HP.class_ $ ClassName "max-w-100 border border-gray-200 shadow-md  bg-white p-5 " ]
      [ HH.div [ HP.class_ $ ClassName "flex items-center" ]
          [ HH.div [ HP.class_ $ ClassName "text-lg font-codeblock font-bold text-pink-700" ]
              [ HH.text libInfo.name
              ]
          , HH.div [ HP.class_ $ ClassName "ml-auto" ]
              [ HH.div [ HP.class_ $ ClassName "flex flex-row-reverse gap-3" ]
                  [ -- document link
                    case libInfo.docs of
                      Just doclink -> do
                        HH.a
                          [ HP.class_ $ ClassName "cursor-pointer flex items-center"
                          , HP.href doclink
                          , HP.target "_blank"
                          , HP.rel "noopener"
                          ]
                          [ HH.img
                              [ HP.class_ $ ClassName " w-4 h-4"
                              , HP.src $ fromAssetURL assets.icons.blackCoverBook
                              ]
                          ]
                      _ -> HH.text ""
                  -- homepage link
                  , HH.a
                      [ HP.class_ $ ClassName "cursor-pointer flex items-center"
                      , HP.href libInfo.home
                      , HP.target "_blank"
                      , HP.rel "noopener"
                      ]
                      [ HH.img
                          [ HP.class_ $ ClassName " w-4 h-4"
                          , HP.src $ fromAssetURL assets.icons.homeBlue
                          ]
                      ]
                  ]

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
      [ HP.class_ $ ClassName "mt-5 text-xs" ]
      [ HH.div [ HP.class_ $ ClassName "flex flex-wrap items-center" ] $
          fold
            [
              -- license mark
              [ HH.div
                  [ HP.class_ $ ClassName "flex items-center" ]
                  [ HH.img
                      [ HP.class_ $ ClassName "w-3 h-3 m-1 "
                      , HP.src $ fromAssetURL assets.icons.law
                      ]
                  , HH.span [ HP.class_ $ ClassName "font-josefin-sans font-bold text-red-900" ]
                      [ HH.text libInfo.license ]
                  ]
              ]
            , [ spacer ]
            , -- purescript compiler version
              case libInfo.type of
                PureScriptLibrary pursVersion -> do
                  [ HH.div [ HP.class_ $ ClassName "flex items-center" ]
                      [ HH.img
                          [ HP.class_ $ ClassName "w-3 h-3 m-1"
                          , HP.src $ fromAssetURL assets.icons.purescript
                          ]
                      , HH.span
                          [ HP.class_ $ ClassName "font-codeblock text-gray-500" ]
                          [ HH.text pursVersion ]
                      ]
                  , spacer
                  ]
                Nix -> do
                  [ HH.div [ HP.class_ $ ClassName "flex items-center" ]
                      [ HH.img
                          [ HP.class_ $ ClassName "w-4 h-4 m-1"
                          , HP.src $ fromAssetURL assets.icons.nix
                          ]
                      ]
                  , spacer
                  ]
                Misc -> []
            , -- status (version)
              [ HH.span
                  [ HP.class_ $ ClassName "cursor-pointer flex items-center"
                  ]
                  [ HH.img
                      [ HP.class_ $ ClassName " w-3 h-3 m-1 "
                      , HP.src $ fromAssetURL assets.icons.tagGreen
                      ]
                  , HH.span [ HP.class_ $ ClassName "font-codeblock text-green-700 " ]
                      [ HH.text case libInfo.status of
                          Unstable -> "beta"
                          Stable ver -> "stable " <> ver
                      ]
                  ]
              ]
            ]
      ]

  spacer = do
    HH.span [ HP.class_ $ ClassName "text-gray-500" ] [ HH.text "・" ]
