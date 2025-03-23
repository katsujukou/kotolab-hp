module Kotolab.HP.API.Schema.Json where

import Prelude

import Data.Argonaut.Core (stringify, Json) as AC
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Codec as C
import Data.Codec.Argonaut as CA
import Data.Either (Either)

stringify :: forall a. CA.JsonCodec a -> a -> String
stringify codec = AC.stringify <<< C.encode codec

parse :: forall a. CA.JsonCodec a -> String -> Either String a
parse codec = jsonParser >=> parseJson codec

parseJson :: forall a. CA.JsonCodec a -> AC.Json -> Either String a
parseJson codec = C.decode codec >>> lmap CA.printJsonDecodeError