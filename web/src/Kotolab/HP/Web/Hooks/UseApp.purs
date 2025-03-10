module Kotolab.HP.Web.Hooks.UseApp where

import Prelude

import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Helix (HelixMiddleware, UseHelix, UseHelixHook, makeStore)
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookType, UseEffect, useLifecycleEffect)
import Halogen.Hooks as Hooks
import Halogen.Subscription as HS
import Kotolab.HP.Web.Routes (Route(..), route)
import Routing.Duplex as RD
import Routing.PushState as PushState
import Unsafe.Coerce (unsafeCoerce)

data AppMode = PC | SmartPhone

derive instance Eq AppMode
derive instance Generic AppMode _
instance Show AppMode where
  show = genericShow

type State =
  { rawPath :: String
  , currentRoute :: Maybe Route
  , mode :: AppMode
  }

data Action = SetRoute { rawPath :: String, route :: Maybe Route }

reducer :: State -> Action -> State
reducer s0 = case _ of
  SetRoute { rawPath, route } -> s0 { rawPath = rawPath, currentRoute = route }

middlewareStack :: forall m. MonadEffect m => HelixMiddleware State Action m
middlewareStack _ act next = do
  case act of
    SetRoute { rawPath } -> do
      liftEffect do
        { pushState } <- PushState.makeInterface
        pushState (unsafeCoerce {}) rawPath
      next act

-- _ -> next act

initialState :: State
initialState =
  { rawPath: ""
  , currentRoute: Just Home
  , mode: SmartPhone
  }

useAppStore :: forall m a. MonadEffect m => Eq a => UseHelixHook State Action a m
useAppStore = makeStore "app" reducer initialState middlewareStack

foreign import data UseApp :: HookType

type UseApp' = UseHelix State <> UseEffect <> Hooks.Pure

instance HookNewtype UseApp UseApp'

type UseAppInterface m =
  { currentRoute :: Maybe Route
  , mode :: AppMode
  , getCurrentRoute :: Hooks.HookM m (Maybe Route)
  , navigateTo :: Route -> Hooks.HookM m Unit
  , getMode :: Hooks.HookM m AppMode
  }

useApp :: forall m. MonadEffect m => Hook m UseApp (UseAppInterface m)
useApp = Hooks.wrap hook
  where
  hook :: _ _ UseApp' _
  hook = Hooks.do
    { currentRoute, mode } /\ storeApi <- useAppStore identity

    useLifecycleEffect do
      let
        updateRoute :: PushState.LocationState -> Hooks.HookM m Unit
        updateRoute lc = do
          storeApi.dispatch $
            SetRoute
              { rawPath: lc.path
              , route: hush $ RD.parse route lc.path
              }

      inst <- liftEffect PushState.makeInterface

      { unlistenLoc, emitter } <- liftEffect do
        { emitter, listener } <- HS.create
        unlistenLoc <- inst.listen (HS.notify listener)
        -- inst.locationState >>= updateRoute
        pure { unlistenLoc, emitter: emitter <#> updateRoute }

      -- 初期化直後のパス情報をStore反映させる
      liftEffect inst.locationState >>= updateRoute

      subscriptionId <- Hooks.subscribe emitter

      pure $ Just (liftEffect unlistenLoc *> Hooks.unsubscribe subscriptionId)

    let
      navigateTo :: Route -> Hooks.HookM m Unit
      navigateTo to = do
        storeApi.dispatch $
          SetRoute { rawPath: RD.print route to, route: Just to }

    Hooks.pure
      { currentRoute
      , mode
      , getCurrentRoute: storeApi.getState <#> _.currentRoute
      , navigateTo
      , getMode: storeApi.getState <#> _.mode
      }