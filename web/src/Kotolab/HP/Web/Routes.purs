module Kotolab.HP.Web.Routes where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = Home
  | Profile
  | Works
  | Contact

derive instance Eq Route
derive instance Generic Route _

route :: RouteDuplex' Route
route = root $ sum
  { "Home": noArgs
  , "Profile": "profile" / noArgs
  , "Works": "work" / noArgs
  , "Contact": "contact" / noArgs
  }