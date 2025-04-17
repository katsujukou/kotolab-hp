module Cloudflare.Workers.Handler.Scheduled where

import Prelude

import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn3, mkEffectFn3)
import Promise (Promise)
import Promise.Aff (fromAff)

foreign import data ScheduledEvent :: Type

foreign import data ExecutionContext :: Type

type ScheduledTrigger env = EffectFn3 ScheduledEvent env ExecutionContext (Promise Unit)

mkScheduledTrigger :: forall env. (ScheduledEvent -> env -> ExecutionContext -> Aff Unit) -> ScheduledTrigger env
mkScheduledTrigger scheduled = mkEffectFn3 \evt env ctx -> fromAff $ scheduled evt env ctx