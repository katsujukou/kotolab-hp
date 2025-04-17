module Kotolab.HP.Backend.Effect.GoogleCalendar where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut (decode)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.Maybe (fromJust)
import Fetch as Fetch
import Foreign.Dayjs as Dayjs
import Foreign.UrlJoin as UrlJoin
import JSURI (encodeURIComponent)
import Kotolab.HP.API.Schema.Types (Datetime)
import Kotolab.HP.Backend.Error (BackendError(..))
import Node.URL as URL
import Node.URL.URLSearchParams as URLSearchParams
import Partial.Unsafe (unsafePartial)
import Run (AFF, Run, EFFECT, liftAff, liftEffect)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

type CalendarId = String

data GoogleCalendar a = GetEventsBetween CalendarId Datetime Datetime (Array Json -> a)

derive instance Functor GoogleCalendar

type GOOGLE_CALENDAR r = (google_calendar :: GoogleCalendar | r)

_google_calendar :: Proxy "google_calendar"
_google_calendar = Proxy

interpret :: forall r a. (GoogleCalendar ~> Run r) -> Run (GOOGLE_CALENDAR + r) a -> Run r a
interpret handler = Run.interpret (Run.on _google_calendar handler Run.send)

fetchHandler :: forall r. { url :: String, apiKey :: String } -> GoogleCalendar ~> Run (EXCEPT BackendError + AFF + EFFECT + r)
fetchHandler config = case _ of
  GetEventsBetween id from to reply -> do
    url <- liftEffect $ URL.new $ UrlJoin.urlJoin
      [ config.url
      , unsafePartial $ fromJust $ encodeURIComponent id
      , "events"
      ]

    searchParams <- liftEffect $ URLSearchParams.toString =<< URLSearchParams.fromObject
      { key: config.apiKey
      , timeMin: Dayjs.format Dayjs.iso8601 from
      , timeMax: Dayjs.format Dayjs.iso8601 to
      , singleEvents: true
      , orderBy: "startTime"
      }

    apiUrl <- liftEffect do
      URL.setSearch searchParams url
      URL.format url

    json <- liftAff do
      resp <- Fetch.fetch apiUrl {}
      resp.json

    let
      apiResp = CA.object "GoogleCalendarApiResponse" $
        CAR.record
          { items: CA.array CA.json
          }

    case decode apiResp (unsafeCoerce json) of
      Left e -> Except.throw $ FailedToDecodeGoogleCalendarApiResponse e
      Right { items } -> pure $ reply items

getEventsBetween :: forall r. CalendarId -> Datetime -> Datetime -> Run (GOOGLE_CALENDAR + r) (Array Json)
getEventsBetween id from to = Run.lift _google_calendar $ GetEventsBetween id from to identity
