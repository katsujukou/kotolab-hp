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
      }
  , images ::
      { selfy :: AssetURL
      , frameLeft :: AssetURL
      }
  }

foreign import assets :: Assets

fromAssetURL :: AssetURL -> String
fromAssetURL = unsafeCoerce