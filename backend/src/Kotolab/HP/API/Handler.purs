module Kotolab.HP.API.Handler where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Dodo as Dodo
import Dodo.Ansi as Ansi
import Effect.Aff (Aff, attempt)
import Effect.Class.Console as Console
import Effect.Exception as Exn
import HTTPurple (ExtRequest, internalServerError, ok, usingCont)
import HTTPurple as HTTPurple
import HTTPurple.Adapter.CloudflareWorkers (CloudflareExt, mkFetch)
import HTTPurple.Adapter.CloudflareWorkers.Fetch (Fetch)
import Kotolab.HP.API.Effect.Hackbar (HACKBAR)
import Kotolab.HP.API.Effect.Hackbar as Hackbar
import Kotolab.HP.API.Effects.Env (ENV)
import Kotolab.HP.API.Effects.Env as Env
import Kotolab.HP.API.Effects.Log (LOG)
import Kotolab.HP.API.Effects.Log as Log
import Kotolab.HP.API.Handlers.HackbarHandler (listHackbarAttendInfoOutput)
import Kotolab.HP.API.Middlewares.Cors (addCorsHeaders)
import Kotolab.HP.API.Middlewares.Restify (RestifyExt, restify)
import Kotolab.HP.API.Schema as Schema
import Kotolab.HP.API.Schema.Json as JSON
import Kotolab.HP.Backend.Cloudflare.Bindings as Bindings
import Kotolab.HP.Backend.Error (BackendError)
import Kotolab.HP.Backend.Error as Error
import Run (AFF, EFFECT, Run, runBaseAff')
import Run.Except (EXCEPT, runExcept)
import Type.Row (type (+))

type ErrorType = BackendError

type ServerEffects = (HACKBAR + ENV + EXCEPT ErrorType + LOG + AFF + EFFECT ())

type ServerExtension = (CloudflareExt Bindings.Env + RestifyExt Schema.Endpoint + ())

router
  :: ExtRequest Schema.Resource ServerExtension
  -> Run ServerEffects HTTPurple.Response
router { endpoint } = usingCont
  case endpoint of
    Schema.Public publicEndpoint -> case publicEndpoint of
      Schema.ListHackbarAttendInfo y m -> do
        output <- do
          hackbarAttendInfo <- lift $ Hackbar.getHackbarAttendList y m
          pure { hackbarAttendInfo }
        ok $ JSON.stringify listHackbarAttendInfoOutput output

fetch :: Fetch Bindings.Env
fetch = mkFetch { route: Schema.resourcePaths, router: runApp (middlewareStack router) }
  where
  runApp router' req = do
    res <- attempt $ runEffects req.cf_env $ router' req
    case res of
      Right resp -> case resp of
        Right a -> pure a
        -- Application-level managed error
        Left err -> do
          Console.log
            $ Dodo.print Ansi.ansiGraphics Dodo.twoSpaces
            $ Log.toLog err
          Error.toResponse err

      -- Unhandled error (a.k.a. panic)
      Left msg -> do
        Console.error $ Exn.message msg
        Console.error $ show $ Exn.stack msg
        internalServerError (Exn.message msg)

  middlewareStack
    :: HTTPurple.MiddlewareM
         (Run ServerEffects)
         Schema.Resource
         (CloudflareExt Bindings.Env ())
         ServerExtension
  middlewareStack = addCorsHeaders <<< restify

  runEffects
    :: forall a
     . Bindings.Env
    -> Run ServerEffects a
    -> Aff (Either ErrorType a)
  runEffects cfEnv m = m
    # Env.interpret (Env.cloudflareBindingHandler cfEnv)
    # Hackbar.interpret (Hackbar.cloudflareKVHandler cfEnv.hackbar_attend_list_dev)
    -- # DB.interpret (DB.cloudflareD1Handler cfEnv."DB")
    # Log.interpret (Log.terminalHandler { minLevel, color: true })
    # runExcept
    # runBaseAff'
    where
    minLevel = case cfEnv."LOG_LEVEL_MIN" of
      "Error" -> Log.Error
      "Warn" -> Log.Warn
      "Debug" -> Log.Debug
      _ -> Log.Info

