module HTTPurple.Adapter.CloudflareWorkers where

import Prelude

import Data.Either (Either(..))
import Effect.Unsafe (unsafePerformEffect)
import HTTPurple as HTTPurple
import HTTPurple.Adapter.CloudflareWorkers.Request (Request, convertRequest)
import HTTPurple.Adapter.CloudflareWorkers.Response (Response, convertResponse)
import Promise.Aff (Promise, fromAff)

mkHandler :: forall route. HTTPurple.BasicRoutingSettings route -> Request -> Promise Response
mkHandler { route, router } req = unsafePerformEffect $ fromAff do
  eitherReq <- convertRequest route req
  httpurpleResp <- case eitherReq of
    Left resp -> resp
    Right httpurpleReq -> router httpurpleReq
  convertResponse httpurpleResp
