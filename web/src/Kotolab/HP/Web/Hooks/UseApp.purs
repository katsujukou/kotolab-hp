module Kotolab.HP.Web.Hooks.UseApp where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen.Helix (UseHelix, UseHelixHook, makeStore')
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookType, UseEffect)
import Halogen.Hooks as Hooks

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

foreign import data UseApp :: HookType

type UseApp' = UseHelix State <> UseEffect <> Hooks.Pure

instance HookNewtype UseApp UseApp'

type UseAppInterface m =
  { mode :: AppMode
  , getMode :: Hooks.HookM m AppMode
  }

useApp :: forall m. MonadEffect m => Hook m UseApp (UseAppInterface m)
useApp = Hooks.wrap hook
  where
  hook :: _ _ UseApp' _
  hook = Hooks.do
    { mode } /\ storeApi <- useAppStore identity

    Hooks.pure
      { mode
      , getMode: storeApi.getState <#> _.mode
      }