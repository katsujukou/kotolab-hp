module Kotolab.HP.Web.Env where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

data Mode = Development | Production | Test

derive instance Eq Mode
derive instance Ord Mode
derive instance Generic Mode _
instance Show Mode where
  show = genericShow

parseMode :: String -> Maybe Mode
parseMode = case _ of
  "development" -> Just Development
  "production" -> Just Production
  "test" -> Just Test
  _ -> Nothing

data Stage = Prod | Dev | Local

derive instance Eq Stage
derive instance Ord Stage
derive instance Generic Stage _
instance Show Stage where
  show = genericShow

parseStage :: String -> Maybe Stage
parseStage = case _ of
  "Prod" -> Just Prod
  "Dev" -> Just Dev
  "Local" -> Just Local
  _ -> Nothing

type Env =
  { stage :: Stage
  , apiBaseURL :: String
  }