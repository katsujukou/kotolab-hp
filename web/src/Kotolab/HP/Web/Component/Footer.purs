module Kotolab.HP.Web.Component.Footer where

import Prelude

import Data.Date (year)
import Data.Enum (fromEnum)
import Effect.Now (nowDate)
import Effect.Unsafe (unsafePerformEffect)
import Fmt as Fmt
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Kotolab.HP.Web.Assets (assets, fromAssetURL)

make :: forall q i o m. H.Component q i o m
make = Hooks.component \_ _ -> Hooks.do
  Hooks.pure $ render {}
  where
  render _ = do
    let
      copywrite = Fmt.fmt @"© {year} by Koto" { year: unsafePerformEffect $ nowDate <#> year >>> fromEnum }

    HH.div [ HP.class_ $ ClassName "flex flex-col items-center text-sm text-gray-500 font-josefin-sans" ]
      [ HH.p [ HP.class_ $ ClassName "my-1" ] [ HH.text copywrite ]
      , HH.p [ HP.class_ $ ClassName "my-1" ]
          [ HH.a
              [ HP.target "_blank"
              , HP.rel "noopener"
              , HP.href "https://github.com/katsujukou/kotolab-hp"
              ]
              [ HH.img
                  [ HP.class_ $ ClassName "h-4 w-4"
                  , HP.src $ fromAssetURL assets.icons.githubMark
                  ]
              ]
          ]
      ]
