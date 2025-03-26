module Kotolab.HP.Web.Component.Types where

import Prelude

import Data.Codec.Argonaut (JsonCodec, prismaticCodec)
import Data.Codec.Argonaut as C
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as Str
import Data.Tuple (Tuple(..))
import Fmt as Fmt
import Foreign.Dayjs as Dayjs
import Kotolab.HP.Web.Routes (Route)

type MenuItem =
  { label :: String
  , route :: Route
  }

data Time = Time Int Int

derive instance Eq Time
instance Show Time where
  show (Time hh mm) = Fmt.fmt @"{hh}:{mm}" { hh: to2Digits hh, mm: to2Digits mm }
    where
    to2Digits n = Str.slice (-3) (-1) $ "000" <> show n <> "0"

type HackbarAttendInfo =
  { date :: Dayjs.Dayjs
  , startTime :: Time
  , endTime :: Time
  }

hackbarAttendInfo :: JsonCodec HackbarAttendInfo
hackbarAttendInfo = CA.object "HackbarAttendInfo" $
  CAR.record
    { date: dayjsCodec
    , startTime: timeCodec
    , endTime: timeCodec
    }
  where
  dayjsCodec = prismaticCodec "Dayjs" Dayjs.parse show C.string

  timeCodec = prismaticCodec "Time" decodeTime encodeTime (CAC.tuple C.int C.int)
    where
    decodeTime (Tuple hh mm)
      | hh >= 0, hh < 60, mm >= 0, mm < 60 = Just (Time hh mm)
      | otherwise = Nothing
    encodeTime (Time hh mm) = Tuple hh mm