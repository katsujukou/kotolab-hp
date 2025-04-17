module Kotolab.HP.Web where

import Prelude

import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception as Exn
import Fmt as Fmt
import Foreign.Object as Object
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Kotolab.HP.Web.App as App
import Kotolab.HP.Web.AppM as AppM
import Kotolab.HP.Web.Env (Env)
import Kotolab.HP.Web.Env as Env
import LambdaKansai.HP.Web.ImportMeta as ImportMeta

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  rootComponent <- runApp
  runUI rootComponent {} body
  where

  runApp :: Aff (H.Component _ _ _ Aff)
  runApp = do
    env <- mkEnv
    pure $ AppM.runApp env App.make

  mkEnv :: Aff Env
  mkEnv = { stage: _, apiBaseURL: _ }
    <$> (getEnv "VITE_APP_STAGE" >>= parseStage)
    <*> (getEnv "VITE_APP_API_BASE_URL")
    where
    getEnv :: String -> Aff String
    getEnv var = liftEffect $ ImportMeta.env >>= Object.lookup var >>>
      maybe
        (Exn.throw $ Fmt.fmt @"Environment Variable {var} is not set!" { var })
        pure

    parseStage :: String -> Aff Env.Stage
    parseStage stg = stg
      # Env.parseStage
      # maybe
          (liftEffect $ Exn.throw $ Fmt.fmt @"{stg} is not a valid stage name" { stg })
          pure