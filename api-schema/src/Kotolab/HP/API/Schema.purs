module Kotolab.HP.API.Schema where

import Prelude hiding ((/))

import Data.Date (Month, Year)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Routing.Duplex (RouteDuplex', int, root, segment)
import Routing.Duplex.Generic (sum)
import Routing.Duplex.Generic.Syntax ((/))

data Resource = HackbarAttend Int Int -- year and month

derive instance Generic Resource _
derive instance Eq Resource
instance Show Resource where
  show = genericShow

resourcePaths :: RouteDuplex' Resource
resourcePaths = root $ sum
  { "HackbarAttend": "hackbar-attend" / int segment / int segment
  }

data Endpoint = Public PublicEndpoint

-- | AdminGuarded AdminApiEndpoint

derive instance Eq Endpoint
derive instance Generic Endpoint _
instance Show Endpoint where
  show = genericShow

data PublicEndpoint = ListHackbarAttendInfo Year Month

derive instance Eq PublicEndpoint
derive instance Generic PublicEndpoint _
instance Show PublicEndpoint where
  show = genericShow

listHackbarAttendInfo :: Year -> Month -> Endpoint
listHackbarAttendInfo y m = Public $ ListHackbarAttendInfo y m

