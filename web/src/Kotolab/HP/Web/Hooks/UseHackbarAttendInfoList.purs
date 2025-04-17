module Kotolab.HP.Web.Hooks.UseHackbarAttendInfoList where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Date (Month, Year)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Foreign.Dayjs as Dayjs
import Halogen.Helix (UseHelix, UseHelixHook, makeStore')
import Halogen.Hooks (class HookNewtype, type (<>), UseState, useState)
import Halogen.Hooks as Hooks
import Kotolab.HP.API.Schema.Types as SchemaTypes
import Kotolab.HP.Web.API as API
import Kotolab.HP.Web.Capabilities.MonadAjax (class MonadAjax)
import Unsafe (unsafeImpure)

type State =
  { value :: Array SchemaTypes.HackbarAttendInfo
  , busy :: Boolean
  }

data Action = SetBusy Boolean | SetValue (Array SchemaTypes.HackbarAttendInfo)

useHackbarAttendInfoListStore
  :: forall m a
   . MonadEffect m
  => Eq a
  => UseHelixHook State Action a m
useHackbarAttendInfoListStore = makeStore' "hackbar-attend-info" reducer initialState
  where
  reducer s0 = case _ of
    SetBusy b -> s0 { busy = b }
    SetValue v -> s0 { value = v }
  initialState =
    { value: []
    , busy: false
    }

foreign import data UseHackbarAttendInfoList :: (Type -> Type) -> Hooks.HookType

type UseHackbarAttendInfoList' m =
  UseHelix State m
    <> UseState
      { year :: Year
      , month :: Month
      }
    <> Hooks.Pure

instance HookNewtype (UseHackbarAttendInfoList m) (UseHackbarAttendInfoList' m)

type HackbarAttendInfoListAPI m =
  { year :: Year
  , month :: Month
  , hackbarAttendInfoList :: Array SchemaTypes.HackbarAttendInfo
  , busy :: Boolean
  , isBusy :: Hooks.HookM m Boolean
  , getHackbarAttendInfoList :: Hooks.HookM m (Array SchemaTypes.HackbarAttendInfo)
  , reload :: Hooks.HookM m Unit
  }

useHackbarAttendInfoList
  :: forall m e
   . MonadEffect m
  => MonadAsk { apiBaseURL :: String | e } m
  => MonadAjax m
  => Hooks.Hook m (UseHackbarAttendInfoList m) (HackbarAttendInfoListAPI m)
useHackbarAttendInfoList = Hooks.wrap hook
  where
  hook :: Hooks.Hook _ (UseHackbarAttendInfoList' m) _
  hook = Hooks.do
    store /\ storeCtx <- useHackbarAttendInfoListStore identity
    ym /\ ymId <- useState $ unsafeImpure do
      day <- Dayjs.now
      let (Just ym) = { year: _, month: _ } <$> Dayjs.year day <*> Dayjs.month day
      pure ym

    let
      reload = do
        { year, month } <- Hooks.get ymId
        attendInfoList <- API.listHackbarAttendInfo year month
        storeCtx.dispatch $ SetValue attendInfoList

    Hooks.pure
      { year: ym.year
      , month: ym.month
      , busy: store.busy
      , hackbarAttendInfoList: store.value
      , isBusy: storeCtx.getState <#> _.busy
      , getHackbarAttendInfoList: storeCtx.getState <#> _.value
      , reload
      }
