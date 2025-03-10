module Kotolab.HP.Web.View.ProfileView where

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
      [ HP.class_ $ ClassName "flex flex-col items-center "
      ]
      [ pageTitle
          { label: "自己紹介"
          , sub: "Profile"
          }
      , HH.div []
          [ HH.div
              [ HP.class_ $ ClassName "text-[2.0rem]"
              ]
              [ HH.text "こと" ]

          , HH.p_
              [ HH.text "神戸三宮のエンジニアが集まるバー "
              , HH.a
                  [ HP.class_ $ ClassName "text-blue-500"
                  , HP.href "https://hackbar.jp/"
                  ]
                  [ HH.text "HACK.BAR" ]
              , HH.text " でバーテンダーをしています🍸️"
              , HH.text "ぜひお話にきてください♪"
              ]
          ]
      ]

