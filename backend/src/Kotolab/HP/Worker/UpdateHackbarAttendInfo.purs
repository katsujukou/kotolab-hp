module Kotolab.HP.Worker.UpdateHackbarAttendInfo
  ( Env
  , TriggerEffects
  , trigger
  ) where

import Prelude

import Cloudflare.KV (KVNamespace)
import Cloudflare.Workers.Handler.Scheduled (ScheduledTrigger, mkScheduledTrigger)
import Data.Array (catMaybes)
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..), hush)
import Data.Enum (fromEnum)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as Str
import Dodo as Dodo
import Dodo.Ansi as Ansi
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Fmt as Fmt
import Foreign.Dayjs as Dayjs
import Kotolab.HP.API.Effect.Hackbar (HACKBAR)
import Kotolab.HP.API.Effect.Hackbar as Hackbar
import Kotolab.HP.API.Schema.Json as JSON
import Kotolab.HP.API.Schema.Json as Json
import Kotolab.HP.API.Schema.Types (Datetime, datetime)
import Kotolab.HP.API.Schema.Types as SchemaTypes
import Kotolab.HP.Backend.Effect.Datetime (DATETIME)
import Kotolab.HP.Backend.Effect.Datetime as Datetime
import Kotolab.HP.Backend.Effect.GoogleCalendar (GOOGLE_CALENDAR)
import Kotolab.HP.Backend.Effect.GoogleCalendar as GoogleCalendar
import Kotolab.HP.Backend.Effects.Log (LOG)
import Kotolab.HP.Backend.Effects.Log as Log
import Kotolab.HP.Backend.Error (BackendError)
import Partial.Unsafe (unsafeCrashWith)
import Run (AFF, EFFECT, Run, liftEffect)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except
import Run.Reader (READER)
import Run.Reader as Reader
import Type.Row (type (+))

type Env =
  { hackbar_attend_list :: KVNamespace
  , "HACKBAR_CALENDAR_GOOGLE_API_KEY" :: String
  , "HACKBAR_GOOGLE_CALENDAR_ID" :: String
  , "GOOGLE_CALENDAR_API_URL" :: String
  }

type GoogleCalendarEvent =
  { summary :: String
  , start :: { dateTime :: Datetime }
  , end :: { dateTime :: Datetime }
  }

googleCalendarEvent :: CA.JsonCodec GoogleCalendarEvent
googleCalendarEvent = CA.object "GoogleCalendarEvent" $
  CAR.record
    { summary: CA.string
    , start: CA.object "GoogleCalendarEvent.start" $
        CAR.record
          { dateTime: datetime
          }
    , end: CA.object "GoogleCalendarEvent.end" $
        CAR.record
          { dateTime: datetime
          }
    }

handler :: Run TriggerEffects Unit
handler = do
  Log.info "invoked!"
  { year, month } <- Datetime.today
  Log.info $ Fmt.fmt @"{y}年{m}月の出勤情報を更新します" { y: fromEnum year, m: fromEnum month }
  let
    to2digits n = Str.slice (-3) (-1) $ "0000" <> show n <> "0"
    mbFrom = Dayjs.parse $ Fmt.fmt @"{y}-{m}-01T00:00:00+09:00" { y: fromEnum year, m: to2digits $ fromEnum month }
    mbTo = Dayjs.parse $ Fmt.fmt @"{y}-{m}-01T00:00:00+09:00" { y: nextYear, m: to2digits nextMonth }
      where
      nextMonth = (1 + fromEnum month) `mod` 12
      nextYear = (fromEnum year) + if nextMonth == 1 then 1 else 0
  events <- case mbFrom, mbTo of
    Just from, Just to -> do
      calendarId <- Reader.asks _."HACKBAR_GOOGLE_CALENDAR_ID"
      GoogleCalendar.getEventsBetween calendarId from to
    _, _ -> unsafeCrashWith "Impossible"

  attendInfoListRef <- liftEffect $ Ref.new []
  for_ events \eventJson -> do
    case Json.parseJson googleCalendarEvent eventJson of
      Right event
        | event.summary == "koto" -> do
            Log.info $ show event
            liftEffect $ Ref.modify_ (_ `Array.snoc` toAttendInfo event) attendInfoListRef
      -- デコード失敗する場合は単にスルーする
      _ -> do
        pure unit
    pure unit
  attendInfoList <- liftEffect $ catMaybes <$> Ref.read attendInfoListRef
  json <- Hackbar.updateHackbarAttendList year month attendInfoList
  Log.info $ "HACKBAR出勤情報を更新しました: " <> json
  pure unit
  where
  toAttendInfo :: GoogleCalendarEvent -> Maybe SchemaTypes.HackbarAttendInfo
  toAttendInfo event = hush do
    startTime <- toAttendTime event.start.dateTime
    endTime <- toAttendTime event.end.dateTime
    pure $
      { date: clearTime event.start.dateTime
      -- Googleカレンダーにはシフトの時間(17:30~ , ~23:30)で記載されているため、営業時間で上下限をつける
      , startTime: max SchemaTypes.openTime startTime
      , endTime: min SchemaTypes.closeTime endTime
      }

  clearTime = Dayjs.setMillisecond bottom
    >>> Dayjs.setSecond bottom
    >>> Dayjs.setMinute bottom
    >>> Dayjs.setHour bottom

  toAttendTime = JSON.parse SchemaTypes.attendTime <<< show <<< Dayjs.format (Dayjs.Format "HH:mm")

type TriggerEffects = (HACKBAR + GOOGLE_CALENDAR + READER Env + DATETIME + LOG + EXCEPT BackendError + AFF + EFFECT + ())

trigger :: ScheduledTrigger Env
trigger = mkScheduledTrigger \_ env _ -> do
  runEffects env handler >>= case _ of
    Right _ -> pure unit
    Left err -> do
      Console.log
        $ Dodo.print Ansi.ansiGraphics Dodo.twoSpaces
        $ Log.toLog err

  where
  runEffects cfEnv m = m
    # Hackbar.interpret (Hackbar.cloudflareKVHandler cfEnv.hackbar_attend_list)
    # GoogleCalendar.interpret
        ( GoogleCalendar.fetchHandler
            { apiKey: cfEnv."HACKBAR_CALENDAR_GOOGLE_API_KEY"
            , url: cfEnv."GOOGLE_CALENDAR_API_URL"
            }
        )
    # Datetime.interpret Datetime.dayjsHandler
    # Log.interpret (Log.terminalHandler { color: true, minLevel: Log.Info })
    # Reader.runReader cfEnv
    # Except.runExcept
    # Run.runBaseAff'