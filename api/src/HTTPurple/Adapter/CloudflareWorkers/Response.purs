module HTTPurple.Adapter.CloudflareWorkers.Response
  ( Response
  , convertResponse
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, runFn2)
import Effect.Aff (Aff, launchAff_, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import HTTPurple as HTTPurple
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
  b <- extractBody resp
  let
    opts = defaultResponse
      { status = resp.status
      , headers = resp.headers
      }
  pure $ runFn2 mkResponseImpl b opts
  where
  defaultResponse =
    { status: 200
    , statusText: ""
    , headers: []
    }
