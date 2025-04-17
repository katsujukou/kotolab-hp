module Foreign.Dayjs
  ( DayOfWeek(..)
  , Dayjs
  , Format(..)
  , date
  , day
  , dayOfWeek
  , format
  , iso8601
  , month
  , now
  , parse
  , setDay
  , setHour
  , setMillisecond
  , setMinute
  , setMonth
  , setSecond
  , setYear
  , year
  ) where

import Prelude

import Data.Date (Day, Month, Year)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), fromEnum, toEnum)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Time (Hour, Millisecond, Minute, Second)
import Effect (Effect)

foreign import data Dayjs :: Type

foreign import showImpl :: Dayjs -> String

instance Show Dayjs where
  show = showImpl

foreign import eqImpl :: Dayjs -> Dayjs -> Boolean

instance Eq Dayjs where
  eq = eqImpl

foreign import now :: Effect Dayjs

foreign import dayImpl :: Dayjs -> Int

data DayOfWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat

derive instance Eq DayOfWeek
derive instance Ord DayOfWeek

derive instance Generic DayOfWeek _
instance Show DayOfWeek where
  show = genericShow

succDayOfWeek :: DayOfWeek -> Maybe DayOfWeek
succDayOfWeek = case _ of
  Sun -> Just Mon
  Mon -> Just Tue
  Tue -> Just Wed
  Wed -> Just Thu
  Thu -> Just Fri
  Fri -> Just Sat
  Sat -> Nothing

prevDayOfWeek :: DayOfWeek -> Maybe DayOfWeek
prevDayOfWeek = case _ of
  Sun -> Nothing
  Mon -> Just Sun
  Tue -> Just Mon
  Wed -> Just Tue
  Thu -> Just Wed
  Fri -> Just Thu
  Sat -> Just Fri

instance Enum DayOfWeek where
  succ = succDayOfWeek
  pred = prevDayOfWeek

instance Bounded DayOfWeek where
  top = Sun
  bottom = Sat

instance BoundedEnum DayOfWeek where
  cardinality = Cardinality 7
  fromEnum = case _ of
    Sun -> 0
    Mon -> 1
    Tue -> 2
    Wed -> 3
    Thu -> 4
    Fri -> 5
    Sat -> 6

  toEnum = case _ of
    0 -> Just Sun
    1 -> Just Mon
    2 -> Just Tue
    3 -> Just Wed
    4 -> Just Thu
    5 -> Just Fri
    6 -> Just Sat
    _ -> Nothing

day :: Dayjs -> Maybe DayOfWeek
day = dayImpl >>> toEnum

date :: Dayjs -> Maybe Day
date = dateImpl >>> toEnum

-- | １始まり。すなわち January = 1
month :: Dayjs -> Maybe Month
month = monthImpl >>> toEnum

year :: Dayjs -> Maybe Year
year = yearImpl >>> toEnum

foreign import dateImpl :: Dayjs -> Int

foreign import monthImpl :: Dayjs -> Int

foreign import yearImpl :: Dayjs -> Int

foreign import parseImpl :: Fn3 (forall a. Maybe a) (forall a. a -> Maybe a) String (Maybe Dayjs)

foreign import unsafeSetImpl :: Fn3 Dayjs String Int Dayjs

setYear :: Year -> Dayjs -> Dayjs
setYear v d = runFn3 unsafeSetImpl d "year" (fromEnum v)

setMonth :: Month -> Dayjs -> Dayjs
setMonth v d = runFn3 unsafeSetImpl d "month" (fromEnum v)

setDay :: Day -> Dayjs -> Dayjs
setDay v d = runFn3 unsafeSetImpl d "date" (fromEnum v)

setHour :: Hour -> Dayjs -> Dayjs
setHour v d = runFn3 unsafeSetImpl d "hour" (fromEnum v)

setMinute :: Minute -> Dayjs -> Dayjs
setMinute v d = runFn3 unsafeSetImpl d "minute" (fromEnum v)

setSecond :: Second -> Dayjs -> Dayjs
setSecond v d = runFn3 unsafeSetImpl d "second" (fromEnum v)

setMillisecond :: Millisecond -> Dayjs -> Dayjs
setMillisecond v d = runFn3 unsafeSetImpl d "millisecond" (fromEnum v)

parse :: String -> Maybe Dayjs
parse = runFn3 parseImpl Nothing Just

foreign import formatImpl :: Fn2 String Dayjs String

format :: Format -> Dayjs -> String
format (Format f) d = runFn2 formatImpl f d

newtype Format = Format String

iso8601 :: Format
iso8601 = Format "YYYY-MM-DDTHH:mm:ss.SSSZ"

dayOfWeek :: Format
dayOfWeek = Format "dddd"