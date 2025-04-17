module Kotolab.HP.API.Middlewares.Cors where

import Prelude

import HTTPurple (Method(..), ok')
import HTTPurple as HTTPurple
import HTTPurple as Headers
import Kotolab.HP.Backend.Effects.Env as Env
import Kotolab.HP.Backend.Effects.Log (LOG)
import Kotolab.HP.API.Middlewares.Utils (withLogger)
import Run (EFFECT, Run, AFF)
import Type.Row (type (+))

addCorsHeaders :: forall route ext r. HTTPurple.MiddlewareM (Run (Env.ENV + EFFECT + AFF + LOG + r)) route ext ext
addCorsHeaders = withLogger "Cors" \_ router req@{ method } -> do
  case method of
    Options -> do
      corsHeaders <- mkCorsHeaders
      ok' corsHeaders ""

    _ -> do
      resp <- router req
      corsHeaders <- mkCorsHeaders
      pure $ resp { headers = corsHeaders }
  where
  mkCorsHeaders = do
    accessControlAllowHeaders <- Env.getAccessControlAllowHeaders
    accessControlAllowMethods <- Env.getAccessControlAllowMethods
    accessControlAllowOrigin <- Env.getAccessControlAllowOrigin
    pure $
      Headers.headers
        { "Access-Control-Allow-Origin": accessControlAllowOrigin
        , "Access-Control-ALlow-Headers": accessControlAllowHeaders
        , "Access-Control-Allow-Methods": accessControlAllowMethods
        }
