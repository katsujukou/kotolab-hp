module Kotolab.HP.API.Schema.Types where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Enum (fromEnum, toEnum)
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.String.CodeUnits as Str
import Data.String.Regex as Re
import Data.String.Regex.Flags (unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Time (Hour, Minute)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Fmt as Fmt
import Foreign.Dayjs (Dayjs)
import Foreign.Dayjs as Dayjs

type Datetime = Dayjs

datetime :: CA.JsonCodec Datetime
datetime = CA.prismaticCodec "Datetime"
  Dayjs.parse
  (Dayjs.format Dayjs.iso8601)
  CA.string

newtype AttendTime = AttendTime (Tuple Hour Minute)

derive instance Eq AttendTime
derive instance Ord AttendTime
instance Show AttendTime where
  show (AttendTime (y /\ m)) = Fmt.fmt @"(AttendTime {year} {month})" { year: show y, month: show m }

printAttendTime :: AttendTime -> String
printAttendTime (AttendTime (h /\ m)) = Fmt.fmt @"{h}:{m}"
  { h: to2DigitString $ fromEnum h
  , m: to2DigitString $ fromEnum m
  }
  where
  to2DigitString n = Str.slice (-3) (-1) $ "0000" <> show n <> "0"

parseAttendTime :: String -> Maybe AttendTime
parseAttendTime s = do
  matched <- Re.match timeRegex s
  hour <- matched NonEmptyArray.!! 1 # join >>= Int.fromString >>= toEnum
  minute <- matched NonEmptyArray.!! 2 # join >>= Int.fromString >>= toEnum
  pure $ AttendTime $ hour /\ minute

  where
  timeRegex = unsafeRegex """^(\d{1,2}):(\d{1,2})$""" unicode

attendTime :: CA.JsonCodec AttendTime
attendTime = CA.prismaticCodec "AttendTime" parseAttendTime printAttendTime CA.string

type HackbarAttendInfo =
  { date :: Dayjs.Dayjs
  , startTime :: AttendTime
  , endTime :: AttendTime
  }

hackbarAttendInfo :: CA.JsonCodec HackbarAttendInfo
hackbarAttendInfo = CA.object "HackbarAttendInfo" $
  CAR.record
    { date: datetime
    , startTime: attendTime
    , endTime: attendTime
    }