module Kotolab.HP.Web.Hooks.UseHeight where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Hooks (class HookNewtype, type (<>), HookType, UseEffect, UseState, useLifecycleEffect, useState)
import Halogen.Hooks as Hooks
import Halogen.Query.Event (eventListener)
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.HTML as HTML
import Web.HTML.Window as Window

foreign import data UseHeight :: HookType

type UseHeight' = UseState Int <> UseEffect <> Hooks.Pure

instance HookNewtype UseHeight UseHeight'

type UseHeightInterface =
  { height :: Int
  }

useHeight :: forall m. MonadEffect m => Hooks.Hook m UseHeight UseHeightInterface
useHeight = Hooks.wrap hook
  where
  hook :: _ _ UseHeight' _
  hook = Hooks.do
    windowHeight /\ windowHeightId <- useState 0

    useLifecycleEffect do
      let
        readHeight :: HTML.Window -> Hooks.HookM m Unit
        readHeight = Hooks.modify_ windowHeightId <<< const <=< liftEffect <<< Window.innerHeight

      window <- liftEffect HTML.window
      subscriptionId <- Hooks.subscribe do
        eventListener
          (EventType "resize")
          (Window.toEventTarget window)
          (Event.target >=> Window.fromEventTarget >>> map readHeight)
      readHeight window
      pure $ Just $ Hooks.unsubscribe subscriptionId

    Hooks.pure
      { height: windowHeight
      }