module Kotolab.HP.Web.AppM where

import Prelude

import Affjax.RequestBody as RequetsBody
import Affjax.RequestHeader as AXH
import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AW
import Affjax.Web as AX
import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Exception as Exn
import Halogen as H
import Kotolab.HP.Web.Capabilities.MonadAjax (class MonadAjax)
import Kotolab.HP.Web.Capabilities.MonadAjax as Ajax
import Kotolab.HP.Web.Env (Env)

newtype AppM a = AppM (ReaderT Env Aff a)

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadAskAppM :: MonadAsk Env AppM

instance monadAjaxAppM :: MonadAjax AppM where
  sendRequest method url mbBody headers = AppM do
    result <- liftAff $ Aff.attempt $ AW.request $
      AW.defaultRequest
        { method = Left method
        , url = url
        , responseFormat = ResponseFormat.string
        , content = mbBody <#> \b -> RequetsBody.string b
        , headers = toAffjaxRequetHeaders headers
        }
    case result of
      Left exn -> liftEffect $ Exn.throwException exn
      Right resp -> case resp of
        Left err -> liftEffect $ throw $ AX.printError err
        Right { status, body } -> pure { statusCode: unwrap status, body }

    where
    toAffjaxRequetHeaders :: Ajax.Headers -> Array AXH.RequestHeader
    toAffjaxRequetHeaders headers' = headers'
      <#> \(Ajax.Header (k /\ vs)) -> AXH.RequestHeader (unwrap k) (joinWith ";" vs)

runApp
  :: forall q i o
   . Env
  -> H.Component q i o AppM
  -> H.Component q i o Aff
runApp env = H.hoist runAppM
  where
  runAppM :: AppM ~> Aff
  runAppM (AppM m) = runReaderT m env