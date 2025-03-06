module Kotolab.HP.API where

import Prelude

import Data.Maybe (fromMaybe)
import Fmt as Fmt
import HTTPurple (Method(..), notFound)
import HTTPurple as HTTPurple
import Kotolab.HP.API.Schema as Schema

main :: HTTPurple.ServerM
main = do
  HTTPurple.serve { hostname: "0.0.0.0", port: 3000 } { route: Schema.route, router }
  where
    router { method, route } = case method, route of 
      Get, Schema.Hello { name } -> do
        HTTPurple.ok $ Fmt.fmt @"Hello, {name}!" { name: fromMaybe "World" name }
      _, _ -> notFound