module Kotolab.HP.Backend.Cloudflare.KV
  ( getJson
  , put
  ) where

import Prelude

import Cloudflare.KV (KVNamespace)
import Data.Argonaut.Core (Json)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import Promise (Promise)
import Promise.Aff (toAffE)

foreign import _getJson :: EffectFn2 KVNamespace String (Promise Json)

getJson :: KVNamespace -> String -> Aff Json
getJson kv = runEffectFn2 _getJson kv >>> toAffE

foreign import _put :: EffectFn3 KVNamespace String String (Promise Unit)

put :: KVNamespace -> String -> String -> Aff Unit
put kv k v = toAffE $ runEffectFn3 _put kv k v
