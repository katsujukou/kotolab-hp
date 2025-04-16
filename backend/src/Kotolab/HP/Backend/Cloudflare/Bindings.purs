module Kotolab.HP.Backend.Cloudflare.Bindings where

import Cloudflare.D1Database (D1Database)
import Cloudflare.KV (KVNamespace)

-- import Cloudflare.KV (KVNamespace)

type Env =
  { "BASE_URL" :: String
  , "FILES_BASE_URL" :: String
  , "ACCESS_CONTROL_ALLOW_HEADERS" :: String
  , "ACCESS_CONTROL_ALLOW_METHODS" :: String
  , "ACCESS_CONTROL_ALLOW_ORIGIN" :: String
  , "LOG_NO_COLOR" :: String
  , "LOG_LEVEL_MIN" :: String
  , "DB" :: D1Database
  , hackbar_attend_list_dev :: KVNamespace
  }