module Kotolab.HP.Web.Component.HTML.PageTitle where

import Prelude

import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Kotolab.HP.Web.Assets (assets, fromAssetURL)

type PageTitleProps =
  { label :: String
  , sub :: String
  }

pageTitle :: forall w i. PageTitleProps -> HH.HTML w i
pageTitle props = do
  HH.div [ HP.class_ $ ClassName "h-24 flex justify-center" ]
    -- [ HH.span [ HP.class_ $ ClassName "h-full flex justify-center items-center" ]
    --     [ HH.div
    --         [ HP.class_ $ ClassName "h-1/2 w-8 bg-yellow-700 filter sepia-100 saturate-100 hue-rotate-0"
    --         , HP.style "mask-image: url('/assets/images/frame-left.svg'); -webkit-mask-image: url('/assets/images/frame-left.svg');"
    --         ]
    --         []
    --     ]

    [ HH.div [ HP.class_ $ ClassName "flex flex-col items-center mx-5 " ]
        [ HH.h1
            [ HP.class_ $ ClassName "text-[2.4rem] text-pink-500 font-allura" ]
            [ HH.text props.sub ]
        , HH.h2
            [ HP.class_ $ ClassName "text-xs text-gray-500 " ]
            [ HH.text "─"
            , HH.span [ HP.class_ $ ClassName "font-nanakyun mx-1" ] [ HH.text props.label ]
            , HH.text "─"
            ]
        ]
    -- , HH.span [ HP.class_ $ ClassName "h-full flex justify-center items-center" ]
    --     [ HH.div
    --         [ HP.class_ $ ClassName "h-1/2 w-8 bg-yellow-700 filter sepia-100 saturate-100 hue-rotate-0 transform scale-x-[-1] "
    --         , HP.style "mask-image: url('/assets/images/frame-left.svg'); -webkit-mask-image: url('/assets/images/frame-left.svg');"
    --         ]
    --         []
    --     ]
    ]