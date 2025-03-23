module Kotolab.HP.API (fetch) where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (fromMaybe)
import Effect.Class.Console as Console
import Fmt as Fmt
import HTTPurple (Method(..), notFound, usingCont)
import HTTPurple as HTTPurple
import HTTPurple.Adapter.CloudflareWorkers (mkHandler)
import HTTPurple.Adapter.CloudflareWorkers.Request (Request)
import HTTPurple.Adapter.CloudflareWorkers.Response (Response)
import Kotolab.HP.API.Schema (route)
import Kotolab.HP.API.Schema as Schema
import Kotolab.HP.API.Schema.Json as Json
import Promise.Aff (Promise)

fetch :: Request -> Promise Response
fetch req = mkHandler { route, router } req
  where
  router req@{ headers, method, route } = usingCont case method, route of
    Get, Schema.Hello { name } -> do
      HTTPurple.ok $ Fmt.fmt @"Hello, {name}!" { name: fromMaybe "World" name }

    Post, Schema.Hello { name } -> do
      Console.logShow headers
      let
        helloInput = CA.object "HelloInput" $ CAR.record { foo: CA.string }
        helloOutput = CA.object "HelloOputput" $ CAR.record { message: CA.string }
      b <- HTTPurple.fromJson (decoder helloInput) req.body
      Console.logShow b
      HTTPurple.ok $ HTTPurple.toJson (encoder helloOutput) { message: Fmt.fmt @"You sent me: {foo}" b }

    _, _ -> notFound

decoder :: forall a. JsonCodec a -> HTTPurple.JsonDecoder String a
decoder codec = HTTPurple.JsonDecoder (Json.parse codec)

encoder :: forall a. JsonCodec a -> HTTPurple.JsonEncoder a
encoder codec = HTTPurple.JsonEncoder (Json.stringify codec)