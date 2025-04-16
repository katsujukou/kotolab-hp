module HTTPurple.Adapter.CloudflareWorkers.Response
  ( Response
  , convertResponse
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.FoldableWithIndex (forWithIndex_)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Effect.Aff (Aff, launchAff_, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import HTTPurple as HTTPurple
import HTTPurple.Adapter.CloudflareWorkers.Headers as Headers
import HTTPurple.Headers as HTTPurple.Headers
import Node.Buffer (Buffer)
import Node.Buffer.Class as Buffer
import Node.EventEmitter as EE
import Node.Stream (dataH, endH)
import Node.Stream as Stream
import Unsafe.Coerce (unsafeCoerce)

foreign import data Response :: Type

foreign import mkResponseImpl :: forall r. Fn2 Buffer { | r } Response

extractBody :: HTTPurple.Response -> Aff Buffer
extractBody { writeBody } = makeAff \done -> do
  stream <- liftEffect Stream.newPassThrough
  -- Consuming body written to stream
  chunks <- liftEffect $ Ref.new []
  stream # EE.on_ dataH \chunk -> do
    Ref.modify_ (\buf -> Array.snoc buf chunk) chunks
  stream # EE.on_ endH do
    body <- Buffer.concat =<< Ref.read chunks
    done $ Right body

  launchAff_ $ writeBody (unsafeCoerce stream)
  pure nonCanceler

convertResponse :: HTTPurple.Response -> Aff Response
convertResponse resp = do
  -- headers
  b <- extractBody resp
  headers <- liftEffect $ convertResponseHeaders resp.headers
  let
    opts =
      { status: resp.status
      , statusText: ""
      , headers
      }
  pure $ runFn2 mkResponseImpl b opts
  where
  convertResponseHeaders (HTTPurple.Headers.ResponseHeaders httpurpleRespHeaders) = do
    headers <- Headers.new []
    forWithIndex_ httpurpleRespHeaders \name value -> do
      Headers.append (unwrap name) (joinWith "," value) headers
    pure headers