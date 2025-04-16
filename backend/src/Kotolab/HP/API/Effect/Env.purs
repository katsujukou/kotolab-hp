module Kotolab.HP.API.Effects.Env where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Exception (throw)
import Fmt as Fmt
import Foreign.Object as Object
import Kotolab.HP.Backend.Cloudflare.Bindings as Cloudflare
import Node.Process as Process
import Run (EFFECT, Run, liftEffect, on, send)
import Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

data Env a = GetEnvVar String (String -> a)

derive instance Functor Env

type ENV r = (env :: Env | r)

_env :: Proxy "env"
_env = Proxy

interpret :: forall r a. (Env ~> Run r) -> Run (ENV + r) a -> Run r a
interpret handler = Run.interpret (on _env handler send)

nodeProcessHandler :: forall r. Env ~> Run (EFFECT + r)
nodeProcessHandler = case _ of
  GetEnvVar varname reply -> do
    envObj <- Run.liftEffect Process.getEnv
    case Object.lookup varname envObj of
      Nothing -> Run.liftEffect $ throw $ "Environment variable " <> varname <> "is not set"
      Just str -> pure $ reply str

cloudflareBindingHandler :: forall r. Cloudflare.Env -> Env ~> Run (EFFECT + r)
cloudflareBindingHandler cfEnv = case _ of
  GetEnvVar varname reply -> case varname of
    "BASE_URL" -> pure $ reply $ cfEnv."BASE_URL"
    "FILES_BASE_URL" -> pure $ reply $ cfEnv."FILES_BASE_URL"
    "ACCESS_CONTROL_ALLOW_HEADERS" -> pure $ reply $ cfEnv."ACCESS_CONTROL_ALLOW_HEADERS"
    "ACCESS_CONTROL_ALLOW_ORIGIN" -> pure $ reply $ cfEnv."ACCESS_CONTROL_ALLOW_ORIGIN"
    "ACCESS_CONTROL_ALLOW_METHODS" -> pure $ reply $ cfEnv."ACCESS_CONTROL_ALLOW_METHODS"
    "LOG_NO_COLOR" -> pure $ reply $ cfEnv."LOG_NO_COLOR"
    "LOG_LEVEL_MIN" -> pure $ reply $ cfEnv."LOG_LEVEL_MIN"
    _ -> liftEffect $ throw $ Fmt.fmt @"Environment variable {varname} is not set." { varname }

getEnvVar :: forall r. String -> Run (ENV + r) String
getEnvVar varname = Run.lift _env $ GetEnvVar varname identity

getCloudFrontSigningKeyName :: forall r. Run (ENV + r) String
getCloudFrontSigningKeyName = getEnvVar "SSM_PARAMETER_CLOUDFRONT_SIGNING_KEY"

getCloudFrontSigningKeyPairId :: forall r. Run (ENV + r) String
getCloudFrontSigningKeyPairId = getEnvVar "CLOUDFRONT_KEY_PAIR_ID"

getBaseURL :: forall r. Run (ENV + r) String
getBaseURL = getEnvVar "BASE_URL"

getFilesBaseURL :: forall r. Run (ENV + r) String
getFilesBaseURL = getEnvVar "FILES_BASE_URL"

getAccessControlAllowHeaders :: forall r. Run (ENV + r) String
getAccessControlAllowHeaders = getEnvVar "ACCESS_CONTROL_ALLOW_HEADERS"

getAccessControlAllowOrigin :: forall r. Run (ENV + r) String
getAccessControlAllowOrigin = getEnvVar "ACCESS_CONTROL_ALLOW_ORIGIN"

getAccessControlAllowMethods :: forall r. Run (ENV + r) String
getAccessControlAllowMethods = getEnvVar "ACCESS_CONTROL_ALLOW_METHODS"
