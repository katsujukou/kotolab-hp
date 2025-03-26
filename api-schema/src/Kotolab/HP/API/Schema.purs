module Kotolab.HP.API.Schema where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Routing.Duplex (RouteDuplex', optional, root, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))

data Route
  = Hello { name :: Maybe String }
  | HackbarAttend

derive instance Eq Route
derive instance Ord Route
derive instance Generic Route _
instance Show Route where
  show = genericShow

route :: RouteDuplex' Route
route = root $ sum
  { "Hello": "hello" ? { name: optional <<< string }
  , "HackbarAttend": "hackbar-attend" / noArgs
  }