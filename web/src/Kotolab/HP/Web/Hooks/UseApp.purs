module Kotolab.HP.Web.Hooks.UseApp where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen.Helix (UseHelix, UseHelixHook, makeStore')
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookType, UseEffect)
import Halogen.Hooks as Hooks
import Kotolab.HP.Web.Hooks.UseNavigate (UseNaviagteInterface)
import Kotolab.HP.Web.Hooks.UseNavigate as UseNavigate
import Kotolab.HP.Web.Routes as Route

data AppMode = PC | SmartPhone

derive instance Eq AppMode
derive instance Generic AppMode _
instance Show AppMode where
  show = genericShow

type State =
  { mode :: AppMode
  }

data Action = None

reducer :: State -> Action -> State
reducer s0 _ = s0

initialState :: State
initialState =
  { mode: PC
  }

useAppStore :: forall m a. MonadEffect m => Eq a => UseHelixHook State Action a m
useAppStore = makeStore' "app" reducer initialState

foreign import data UseApp :: (Type -> Type) -> HookType

type UseApp' m = UseHelix State m <> UseNavigate.UseNavigate m Route.Route <> UseEffect <> Hooks.Pure

instance HookNewtype (UseApp m) (UseApp' m)

type UseAppInterface m =
  { mode :: AppMode
  , getMode :: Hooks.HookM m AppMode
  , initialize :: Hooks.HookM m Unit
  }

useApp :: forall m. MonadEffect m => Hook m (UseApp m) (UseAppInterface m)
useApp = Hooks.wrap hook
  where
  hook :: _ _ (UseApp' _) _
  hook = Hooks.do
    { mode } /\ storeApi <- useAppStore identity
    navigator <- useNavigate
    let
      initialize = do
        navigator.initialize

    Hooks.pure
      { mode
      , getMode: storeApi.getState <#> _.mode
      , initialize
      }

useNavigate :: forall m. MonadEffect m => Hooks.Hook m (UseNavigate.UseNavigate m Route.Route) (UseNaviagteInterface Route.Route m)
useNavigate = UseNavigate.useNavigate Route.route