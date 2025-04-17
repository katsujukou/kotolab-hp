module Kotolab.HP.Web.API.Util where

import Prelude

import Control.Monad.Reader (class MonadAsk, ask)
import Data.Array (fold)
import Data.Bifunctor (rmap)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Tuple (uncurry)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception as Exn
import Fmt as Fmt
import Kotolab.HP.API.Schema.Json as Json
import Kotolab.HP.API.Schema as Schema
import Kotolab.HP.API.Schema.Printer as Printer
import Kotolab.HP.Web.Capabilities.MonadAjax (class MonadAjax, (:))
import Kotolab.HP.Web.Capabilities.MonadAjax as Ajax

parseJson
  :: forall m a
   . MonadEffect m
  => CA.JsonCodec a
  -> String
  -> m a
parseJson codec = Json.parse codec >>> either (Exn.throw >>> liftEffect) pure

sendApiRequest'
  :: forall m env a b
   . MonadAjax m
  => MonadAsk { apiBaseURL :: String | env } m
  => { encoder :: Maybe (a -> String), decoder :: String -> m b }
  -> Maybe String
  -> Schema.Endpoint
  -> Maybe a
  -> m (Either { statusCode :: Int, body :: String } b)
sendApiRequest' { encoder, decoder } token' endpoint a = do
  let body = encoder <*> a
  { apiBaseURL } <- ask
  resp <- uncurry Ajax.sendRequest
    (rmap (apiBaseURL <> _) $ Printer.print endpoint)
    body
    headers
  case resp.statusCode of
    200 -> Right <$> decoder resp.body
    _ -> pure $ Left resp
  where

  headers = fold
    [ [ "Content-Type" : "application/json"
      , "Accept" : "application/json"
      , "X-Requested-With" : "XMLHttpRequest"
      ]
    , authHeaders token'
    ]
  authHeaders = case _ of
    Nothing -> []
    Just token -> [ "Authorization" : Fmt.fmt @"Bearer {token}" { token } ]

sendApiRequest
  :: forall m env a b
   . MonadAjax m
  => MonadAsk { apiBaseURL :: String | env } m
  => { encoder :: Maybe (a -> String), decoder :: String -> m b }
  -> Maybe String
  -> Schema.Endpoint
  -> Maybe a
  -> m b
sendApiRequest ed token endpoint a = do
  sendApiRequest' ed token endpoint a
    >>= either errorHandler pure
  where
  errorHandler = show >>> ("Failed to send API request: " <> _) >>> Exn.throw >>> liftEffect