module Kotolab.HP.Admin.App where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks

make :: forall q i o m. H.Component q i o m
make = Hooks.component \_ _ -> Hooks.do
  let
    ctx = {}
  Hooks.pure $ render ctx
  where
  render ctx = do
    HH.div []
      [ HH.text "ことさんち 管理画面" ]