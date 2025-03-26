module Foreign.Dayjs
  ( DayOfWeek(..)
  , Dayjs
  , codec
  , date
  , day
  , format
  , month
  , now
  , parse
  , year
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec, prismaticCodec)
import Data.Codec.Argonaut as C
import Data.Date (Day, Month, Year)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), toEnum)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
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

month :: Dayjs -> Maybe Month
month = monthImpl >>> toEnum

year :: Dayjs -> Maybe Year
year = yearImpl >>> toEnum

foreign import dateImpl :: Dayjs -> Int

foreign import monthImpl :: Dayjs -> Int

foreign import yearImpl :: Dayjs -> Int

foreign import parseImpl :: Fn3 (forall a. Maybe a) (forall a. a -> Maybe a) String (Maybe Dayjs)

parse :: String -> Maybe Dayjs
parse = runFn3 parseImpl Nothing Just

foreign import formatImpl :: Fn2 String Dayjs String

format :: String -> Dayjs -> String
format = runFn2 formatImpl

codec :: JsonCodec Dayjs
codec = prismaticCodec "Dayjs" parse show C.string