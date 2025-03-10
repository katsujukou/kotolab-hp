module Kotolab.HP.Web.Component.Types where

import Kotolab.HP.Web.Routes (Route)

type MenuItem =
  { label :: String
  , route :: Route
  }