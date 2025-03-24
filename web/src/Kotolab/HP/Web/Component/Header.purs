module Kotolab.HP.Web.Component.Header where

import Prelude

import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks

make :: forall q i o m. H.Component q i o m
make = Hooks.component \_ _ -> Hooks.do
  Hooks.pure $ render {}
  where
  render _ = do
    HH.div [ HP.class_ $ ClassName "flex items-center justify-center p-3" ]
      [ HH.text "ことさんち"
      ]