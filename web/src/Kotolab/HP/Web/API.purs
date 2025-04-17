module Kotolab.HP.Web.API where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Date (Month, Year)
import Data.Maybe (Maybe(..))
import Kotolab.HP.API.Schema as Schema
import Kotolab.HP.API.Schema.Types as SchemaTypes
import Kotolab.HP.Web.API.Util (parseJson, sendApiRequest)
import Kotolab.HP.Web.Capabilities.MonadAjax (class MonadAjax)

type Input = Maybe String

type Output = String

listHackbarAttendInfo
  :: forall m env
   . MonadAsk { apiBaseURL :: String | env } m
  => MonadAjax m
  => Year
  -> Month
  -> m (Array SchemaTypes.HackbarAttendInfo)
listHackbarAttendInfo y m = do
  { hackbarAttendInfo } <- sendApiRequest
    { encoder: Nothing
    , decoder: parseJson
        ( CA.object "ListHackbarAttendInfoApiReponse" $
            CAR.record { hackbarAttendInfo: CA.array SchemaTypes.hackbarAttendInfo }
        )
    }
    Nothing
    (Schema.listHackbarAttendInfo y m)
    Nothing
  pure hackbarAttendInfo
