module Kotolab.HP.API.Schema.Printer where

import Prelude

import Data.Bifunctor (rmap)
import Data.Enum (fromEnum)
import Data.HTTP.Method (Method(..))
import Data.Tuple.Nested (type (/\), (/\))
import Kotolab.HP.API.Schema (Endpoint(..), PublicEndpoint(..), Resource(..), resourcePaths)
import Routing.Duplex as RD

print :: Endpoint -> Method /\ String
print = case _ of
  Public endpoint -> printPublicEndpoint endpoint
  where
  printPublicEndpoint = rmap (RD.print resourcePaths)
    <<< case _ of
      ListHackbarAttendInfo y m -> GET /\ HackbarAttend (fromEnum y) (fromEnum m)