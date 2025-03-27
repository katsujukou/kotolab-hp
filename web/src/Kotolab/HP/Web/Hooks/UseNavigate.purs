module Kotolab.HP.Web.Hooks.UseNavigate where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..))
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

type State =
  { rawPath :: String
  , currentRoute :: Maybe Route
  , initialized :: Boolean
  }

data Action = Initialized | SetRoute { rawPath :: String, route :: Maybe Route }

reducer :: State -> Action -> State
reducer s0 = case _ of
  Initialized -> s0 { initialized = true }
  SetRoute { rawPath, route } -> s0 { rawPath = rawPath, currentRoute = route }

middlewareStack :: forall m. MonadEffect m => HelixMiddleware State Action m
middlewareStack _ act next = do
  case act of
    SetRoute { rawPath } -> do
      liftEffect do
        { pushState, locationState } <- PushState.makeInterface
        loc <- locationState
        when (loc.pathname /= rawPath) do
          pushState (unsafeCoerce {}) rawPath
      next act
    _ -> next act

initialState :: State
initialState =
  { rawPath: ""
  , currentRoute: Just Home
  , initialized: false
  }

useNavigateStore :: forall m a. MonadEffect m => Eq a => UseHelixHook State Action a m
useNavigateStore = makeStore "navigate" reducer initialState middlewareStack

foreign import data UseApp :: HookType

type UseApp' = UseHelix State <> UseEffect <> Hooks.Pure

instance HookNewtype UseApp UseApp'

type UseNaviagteInterface m =
  { currentRoute :: Maybe Route
  , getCurrentRoute :: Hooks.HookM m (Maybe Route)
  , navigateTo :: Route -> Hooks.HookM m Unit
  }

useNavigate :: forall m. MonadEffect m => Hook m UseApp (UseNaviagteInterface m)
useNavigate = Hooks.wrap hook
  where
  hook :: _ _ UseApp' _
  hook = Hooks.do
    { currentRoute } /\ storeApi <- useNavigateStore identity

    useLifecycleEffect $ do
      initialized <- storeApi.getState <#> _.initialized
      if initialized then pure Nothing
      else do
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
          pure { unlistenLoc, emitter: emitter <#> updateRoute }

        -- 初期化直後のパス情報をStore反映させる
        liftEffect inst.locationState >>= updateRoute

        subscriptionId <- Hooks.subscribe emitter

        storeApi.dispatch Initialized

        pure $ Just do
          liftEffect unlistenLoc
          Hooks.unsubscribe subscriptionId

    let
      navigateTo :: Route -> Hooks.HookM m Unit
      navigateTo to = do
        storeApi.dispatch $
          SetRoute { rawPath: RD.print route to, route: Just to }

    Hooks.pure
      { currentRoute
      , getCurrentRoute: storeApi.getState <#> _.currentRoute
      , navigateTo
      }