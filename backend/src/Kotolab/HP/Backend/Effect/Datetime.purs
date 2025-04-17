module Kotolab.HP.Backend.Effect.Datetime
  ( DATETIME
  , Datetime
  , DatetimeF(..)
  , _datetime
  , dayjsHandler
  , interpret
  , today
  ) where

import Prelude

import Data.Date (Day, Month, Weekday, Year)
import Data.Enum (toEnum)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Foreign.Dayjs (Dayjs)
import Foreign.Dayjs as Dayjs
import Partial.Unsafe (unsafePartial)
import Run (Run, EFFECT)
import Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

-- #FIXME dayjsにダイレクトに依存しているのが微妙

type Datetime = Dayjs

data DatetimeF a = Today ({ year :: Year, month :: Month, day :: Day, weekday :: Weekday } -> a)

derive instance Functor DatetimeF

type DATETIME r = (datetime :: DatetimeF | r)

_datetime :: Proxy "datetime"
_datetime = Proxy

interpret :: forall r a. (DatetimeF ~> Run r) -> Run (DATETIME + r) a -> Run r a
interpret handler = Run.interpret (Run.on _datetime handler Run.send)

dayjsHandler :: forall r. DatetimeF ~> Run (EFFECT + r)
dayjsHandler = case _ of
  Today reply -> do
    now <- Run.liftEffect $ Dayjs.now
    let
      mbymd = { year: _, month: _, day: _, weekday: _ }
        <$> Dayjs.year now
        <*> Dayjs.month now
        <*> Dayjs.date now
        -- Dayjsのフォーマット(d)は、日曜０始まりだが、Weekdayは Monday 1 ~ Sunday 7なので...
        <*> (Dayjs.format (Dayjs.Format "d") now # Int.fromString >>= toEnum @Weekday)
    unsafePartial case mbymd of
      Just ymd -> pure $ reply ymd

today :: forall r. Run (DATETIME + r) { year :: Year, month :: Month, day :: Day, weekday :: Weekday }
today = Run.lift _datetime $ Today identity
