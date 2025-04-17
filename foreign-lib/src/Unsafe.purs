module Unsafe where

import Prelude

import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)

unsafeImpure :: forall a. (Partial => Effect a) -> a
unsafeImpure = unsafePerformEffect <<< unsafePartial