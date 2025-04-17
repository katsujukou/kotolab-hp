module Kotolab.HP.API.Middlewares.Utils where

import Prelude

import Dodo (text)
import Dodo.Ansi (bold)
import Fmt as Fmt
import HTTPurple as HTTPurple
import Kotolab.HP.Backend.Effects.Log (class Loggable, LOG, LogLevel(..), green, toLog)
import Kotolab.HP.Backend.Effects.Log as Log
import Run (Run)
import Type.Row (type (+))

middlewareLog :: forall r a. Loggable a => String -> LogLevel -> a -> Run (LOG + r) Unit
middlewareLog id level msg = do
  let
    header = text "[middleware: " <> (bold $ green id) <> text "] "
  Log.log level $ header <> toLog msg

withLogger
  :: forall r route extIn extOut
   . String
  -> ((forall a. Loggable a => LogLevel -> a -> Run (LOG + r) Unit) -> HTTPurple.MiddlewareM (Run (LOG + r)) route extIn extOut)
  -> HTTPurple.MiddlewareM (Run (LOG + r)) route extIn extOut
withLogger id middleware = \router req -> do
  middlewareLog id Info "Enter."
  resp <- middleware (middlewareLog id) router req
  middlewareLog id Info (Fmt.fmt @"Leave (statusCode={statusCode})" { statusCode: resp.status })
    $> resp