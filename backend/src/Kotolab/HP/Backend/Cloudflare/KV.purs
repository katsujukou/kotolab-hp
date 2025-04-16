module Kotolab.HP.Backend.Cloudflare.KV where

import Prelude

import Cloudflare.KV (KVNamespace)
import Data.Argonaut.Core (Json)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Promise (Promise)
import Promise.Aff (toAffE)

foreign import _getJson :: EffectFn2 KVNamespace String (Promise Json)

getJson :: KVNamespace -> String -> Aff Json
getJson kv = runEffectFn2 _getJson kv >>> toAffE
