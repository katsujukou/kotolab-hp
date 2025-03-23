module HTTPurple.Adapter.CloudflareWorkers.Headers where

import Data.Array.NonEmpty (NonEmptyArray)

foreign import data Headers :: Type

foreign import entries :: Headers -> Array (NonEmptyArray String)
