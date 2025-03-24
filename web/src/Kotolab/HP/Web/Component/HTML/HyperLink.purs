module Kotolab.HP.Web.Component.HTML.HyperLink where

import Prelude

import Halogen.HTML as HH

type Props =
  { innerHTML :: forall w i. HH.HTML w i
  , innerText :: String
  , href :: String
  }

render :: forall w i. Props -> HH.HTML w i
render props = do
  HH.a
    []
    []