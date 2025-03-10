module Kotolab.HP.Web.View.HomeView where

import Prelude

import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Kotolab.HP.Web.Component.HTML.PageTitle (pageTitle)

make :: forall q i o m. H.Component q i o m
make = Hooks.component \_ _ -> Hooks.do
  Hooks.pure (render {})
  where
  render _ = do
    HH.div
      [ HP.class_ $ ClassName "flex flex-col items-center"
      ]
      [ pageTitle
          { label: "ことさんちへ　ようこそ！"
          , sub: ""
          }
      , HH.div
          [ HP.class_ $ ClassName ""
          ]
          [ HH.p []
              [ HH.text "ここは こと のホームページです。"
              ]
          ]
      ]