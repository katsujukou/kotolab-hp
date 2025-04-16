module HTTPurple.Adapter.CloudflareWorkers.Headers
  ( Headers
  , append
  , entries
  , new
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn3, runEffectFn1, runEffectFn3)

foreign import data Headers :: Type

foreign import entries :: Headers -> Array (NonEmptyArray String)

foreign import appendImpl :: EffectFn3 String String Headers Unit

append :: String -> String -> Headers -> Effect Unit
append = runEffectFn3 appendImpl

foreign import newImpl :: EffectFn1 (Array (Array String)) Headers

new :: Array (String /\ String) -> Effect Headers
new init = runEffectFn1 newImpl (uncurry (\k v -> [ k, v ]) <$> init)