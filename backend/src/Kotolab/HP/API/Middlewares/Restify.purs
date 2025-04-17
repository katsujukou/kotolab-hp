module Kotolab.HP.API.Middlewares.Restify where

import Prelude

import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..))
import Fmt as Fmt
import HTTPurple (Method(..), RequestR, methodNotAllowed, unprocessableEntity)
import HTTPurple as HTTPurple
import Kotolab.HP.Backend.Effects.Log (LOG, LogLevel(..))
import Kotolab.HP.API.Middlewares.Utils (withLogger)
import Kotolab.HP.API.Schema as Schema
import Prim.Row as Row
import Record.Studio ((//))
import Run (EFFECT, Run, AFF)
import Type.Row (type (+))

type RestifyExt :: Type -> Row Type -> Row Type
type RestifyExt endpoint r = (endpoint :: endpoint | r)

restify
  :: forall r extIn extOut
   . Row.Nub (RequestR Schema.Resource extOut) (RequestR Schema.Resource extOut)
  => Row.Union extIn (endpoint :: Schema.Endpoint) extOut
  => HTTPurple.MiddlewareM (Run (LOG + AFF + EFFECT + r)) Schema.Resource extIn extOut
restify = withLogger "Restify" \logger router req@{ method } -> do
  case toEndpoint method req.route of
    Right endpoint -> do
      logger Info $
        Fmt.fmt @"Endpoint: {endpoint}" { endpoint: show endpoint }
      router ({ endpoint } // req)
    Left errResp -> errResp
  where
  toEndpoint method = case _ of
    Schema.HackbarAttend y m -> case method of
      Get
        | y >= 2024 -- HACKBARでお給仕しだしたのは2024年1月だから、それより前の出勤情報はあるわけない
        , Just year <- toEnum y
        , Just month <- toEnum m -> Right $ Schema.listHackbarAttendInfo year month
        | otherwise -> Left unprocessableEntity
      _ -> Left methodNotAllowed

