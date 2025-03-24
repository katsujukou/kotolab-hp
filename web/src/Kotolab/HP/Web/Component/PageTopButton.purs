module Kotolab.HP.Web.Component.PageTopButton where

import Prelude

import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Kotolab.HP.Web.Assets (assets, fromAssetURL)

data Output = Clicked

make :: forall q i m. H.Component q i Output m
make = Hooks.component \{ outputToken } _ -> Hooks.do

  let
    handleClicked = do
      Hooks.raise outputToken Clicked

    ctx = { handleClicked }

  Hooks.pure $ render ctx
  where
  render ctx = do
    HH.div
      [ HP.class_ $ ClassName "w-10 h-10 cursor-pointer"
      , HE.onClick \_ -> ctx.handleClicked
      ]
      [ HH.img
          [ HP.class_ $ ClassName ""
          , HP.src $ fromAssetURL assets.icons.backToTop
          ]
      ]