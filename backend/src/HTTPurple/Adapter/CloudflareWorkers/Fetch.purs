module HTTPurple.Adapter.CloudflareWorkers.Fetch where

import Prelude

import Data.Function.Uncurried (Fn3, mkFn3)
import Effect.Aff (Aff)
import Effect.Unsafe (unsafePerformEffect)
import HTTPurple.Adapter.CloudflareWorkers.Context (Context)
import HTTPurple.Adapter.CloudflareWorkers.Request (Request)
import HTTPurple.Adapter.CloudflareWorkers.Response (Response)
import Promise (Promise)
import Promise.Aff (fromAff)

type Fetch env = Fn3 Request env Context (Promise Response)

toFetch :: forall env. (Request -> env -> Context -> Aff Response) -> Fetch env
toFetch f = mkFn3 \req env ctx -> unsafePerformEffect $ fromAff $ f req env ctx
