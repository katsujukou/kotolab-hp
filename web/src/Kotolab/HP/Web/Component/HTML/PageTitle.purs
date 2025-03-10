module Kotolab.HP.Web.Component.HTML.PageTitle where

import Prelude

import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type PageTitleProps =
  { label :: String
  , sub :: String
  }

pageTitle :: forall w i. PageTitleProps -> HH.HTML w i
pageTitle props = do
  HH.div [ HP.class_ $ ClassName "flex flex-col items-center m-3 " ]
    [ HH.h1
        [ HP.class_ $ ClassName "text-pink-500 text-lg" ]
        [ HH.text props.label ]
    , HH.h2
        [ HP.class_ $ ClassName "text-gray-500" ]
        [ HH.text props.sub ]
    ]
