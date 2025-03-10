module Kotolab.HP.Web.Assets where

import Unsafe.Coerce (unsafeCoerce)

foreign import data AssetURL :: Type

foreign import assets
  :: { icons ::
         { menu :: AssetURL
         , close :: AssetURL
         , githubMark :: AssetURL
         , twitterLogo :: AssetURL
         , iconEmail :: AssetURL
         }
     }

fromAssetURL :: AssetURL -> String
fromAssetURL = unsafeCoerce