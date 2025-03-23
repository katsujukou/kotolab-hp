module Kotolab.HP.Web.Component.HackbarAttendInfo where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks

make :: forall q i o m. H.Component q i o m
make = Hooks.component \_ _ -> Hooks.do
  Hooks.pure (render {})
  where
  render ctx = do
    HH.div []
      [ HH.text "今週のHACK.BAR出勤情報" ]