module HTTPurple.Adapter.CloudflareWorkers.Request where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import HTTPurple (RouteDuplex')
import HTTPurple as HTTPurple
import HTTPurple as RD
import HTTPurple.Adapter.CloudflareWorkers.Headers (Headers)
import HTTPurple.Adapter.CloudflareWorkers.Headers as Headers
import HTTPurple.Body as HTTPurple.RequestBody
import HTTPurple.Headers as HTTPurpleHeaders
import HTTPurple.Method as Method
import HTTPurple.Path as Path
import HTTPurple.Query as Query
import HTTPurple.Version as Version
import Node.Encoding (Encoding(..))
import Node.Stream (readableFromString)
import Node.URL as URL
import Promise (Promise)
import Promise.Aff (toAffE)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Request :: Type

foreign import headersImpl :: Request -> Headers

foreign import methodImpl :: Request -> String

foreign import urlImpl :: Request -> String

foreign import bodyTextImpl :: Request -> Effect (Promise String)

convertRequest :: forall route. RouteDuplex' route -> Request -> Aff (Either HTTPurple.ResponseM (HTTPurple.Request route))
convertRequest routes jsReq = do
  let method = Method.read (unsafeCoerce { method: methodImpl jsReq })
  body <- convertBody jsReq
  { url, query, path, pathname } <- liftEffect $ convertURL (urlImpl jsReq)
  let headers = convertHeaders $ headersImpl jsReq
  case RD.parse routes pathname of
    Left _ -> pure $ Left HTTPurple.notFound
    Right route -> do
      pure $ Right
        { method
        , headers
        , url: url
        , body
        , route
        , path
        , query
        , httpVersion: Version.Other "Unknown"
        }
  where
  convertURL jsUrl = do
    url <- URL.new jsUrl
    pathname <- URL.pathname url
    search <- URL.search url
    pure
      { url: jsUrl
      , path: Path.read (unsafeCoerce { url: jsUrl })
      , query: Query.read (unsafeCoerce { url: jsUrl })
      , pathname: pathname <> search
      }

  convertHeaders jsHeaders = do
    jsHeaders
      # Headers.entries
      <#> (NonEmptyArray.uncons >>> \{ head, tail } -> head /\ joinWith ", " tail)
      # HTTPurpleHeaders.mkRequestHeaders

  convertBody :: _ -> _ HTTPurple.RequestBody.RequestBody
  convertBody req = do
    buffer <- liftEffect $ Ref.new Nothing
    bodyString <- toAffE $ bodyTextImpl req
    string <- liftEffect $ Ref.new (Just bodyString)
    stream <- liftEffect $ readableFromString bodyString UTF8
    pure { buffer, string, stream }