module Kotolab.HP.Web.Assets where

import Unsafe.Coerce (unsafeCoerce)

foreign import data AssetURL :: Type

type Assets =
  { icons ::
      { menu :: AssetURL
      , close :: AssetURL
      , githubMark :: AssetURL
      , twitterLogo :: AssetURL
      , iconEmail :: AssetURL
      , blackCoverBook :: AssetURL
      , law :: AssetURL
      , homeBlue :: AssetURL
      , tagGreen :: AssetURL
      , packageRibboned :: AssetURL
      , backToTop :: AssetURL
      , purescript :: AssetURL
      , nix :: AssetURL
      }
  , images ::
      { selfy :: AssetURL
      , frameLeft :: AssetURL
      , roseFrame :: AssetURL
      , roseFrameAngle :: AssetURL
      }
  }

foreign import assets :: Assets

fromAssetURL :: AssetURL -> String
fromAssetURL = unsafeCoerce