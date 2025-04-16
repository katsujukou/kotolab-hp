module HTTPurple.Adapter.CloudflareWorkers where

import Prelude

import Data.Either (Either(..))
import HTTPurple as HTTPurple
import HTTPurple.Adapter.CloudflareWorkers.Context (Context)
import HTTPurple.Adapter.CloudflareWorkers.Fetch (Fetch, toFetch)
import HTTPurple.Adapter.CloudflareWorkers.Request (convertRequest)
import HTTPurple.Adapter.CloudflareWorkers.Response (convertResponse)
import Record.Studio ((//))

type CloudflareExt env ext = (cf_env :: env, cf_ctx :: Context | ext)

mkFetch :: forall env route. { | HTTPurple.RoutingSettingsR route (CloudflareExt env ()) () } -> Fetch env
mkFetch { route, router } = toFetch \req cf_env cf_ctx -> do
  eitherReq <- convertRequest route req
  httpurpleResp <- case eitherReq of
    Left resp -> resp
    Right httpurpleReq -> router (httpurpleReq // { cf_env, cf_ctx })
  convertResponse httpurpleResp
