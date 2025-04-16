module Kotolab.HP.API.Handlers.HackbarHandler where

import Prelude

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Kotolab.HP.API.Schema.Types as SchemaTypes

type ListHackbarAttendInfoOutput =
  { hackbarAttendInfo :: Array SchemaTypes.HackbarAttendInfo }

listHackbarAttendInfoOutput :: CA.JsonCodec ListHackbarAttendInfoOutput
listHackbarAttendInfoOutput =
  CA.object "ListHackbarAttendInfoOutput"
    $ CAR.record
        { hackbarAttendInfo: CA.array $ SchemaTypes.hackbarAttendInfo }
