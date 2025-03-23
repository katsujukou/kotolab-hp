module Kotolab.HP.Web where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Kotolab.HP.Web.App as App
import Kotolab.HP.Web.AppM as AppM
import Kotolab.HP.Web.Env (Env)

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

  mkEnv :: _ Env
  mkEnv = pure {}