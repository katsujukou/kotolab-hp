module Kotolab.HP.Web.Component.HTML.Badge where

import Prelude

import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type PureScriptBadgeProps =
  { version :: String
  }

-- purescriptBadge :: forall w i. PureScriptBadgeProps -> HH.HTML w i
-- purescriptBadge props = do
--   HH.a []
--     [ HH.img
--        [ HP.src "https://camo.githubusercontent.com/dc6eb9f5ecd5d59310c54e86c0c09e305c6528e750f20570660caa7e2f188a37/68747470733a2f2f696d672e736869656c64732e696f2f62616467652f707572732d76302e31352e31352d626c75653f6c6f676f3d70757265736372697074" alt="purs - v0.15.15" data-canonical-src="https://img.shields.io/badge/purs-v0.15.15-blue?logo=purescript" ]
--     ]
-- <a href="https://github.com/purescript/purescript/releases/tag/v0.15.15">
--   <img src="https://camo.githubusercontent.com/dc6eb9f5ecd5d59310c54e86c0c09e305c6528e750f20570660caa7e2f188a37/68747470733a2f2f696d672e736869656c64732e696f2f62616467652f707572732d76302e31352e31352d626c75653f6c6f676f3d70757265736372697074" alt="purs - v0.15.15" data-canonical-src="https://img.shields.io/badge/purs-v0.15.15-blue?logo=purescript" style="max-width: 100%;">
-- </a>
