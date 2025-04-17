module Kotolab.HP.Backend.Effects.Log where

import Prelude

import Ansi.Codes (GraphicsParam)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Dodo (Doc)
import Dodo as Dodo
import Dodo.Ansi (foreground)
import Dodo.Ansi as Ansi
import Effect.Console as Console
import Effect.Exception as Exn
import Run (EFFECT, Run, liftEffect)
import Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

data LogLevel = Debug | Info | Warn | Error

derive instance Generic LogLevel _
derive instance Eq LogLevel
derive instance Ord LogLevel
instance Show LogLevel where
  show = genericShow

class Loggable a where
  toLog :: a -> Doc GraphicsParam

instance Loggable (Doc GraphicsParam) where
  toLog = identity

instance Loggable String where
  toLog = Dodo.text

instance Loggable a => Loggable (Maybe a) where
  toLog = case _ of
    Nothing -> Dodo.text "Nothing"
    Just a -> Dodo.text "("
      <> (foreground Ansi.Blue (Dodo.text "Just "))
      <> toLog a
      <> (Dodo.text ")")

data Log a = Log LogLevel (Dodo.Doc GraphicsParam) a

derive instance Functor Log

type LOG r = (log :: Log | r)

_log :: Proxy "log"
_log = Proxy

interpret :: forall r a. (Log ~> Run r) -> Run (LOG + r) a -> Run r a
interpret handler = Run.interpret (Run.on _log handler Run.send)

type LoggerConfig = { minLevel :: LogLevel, color :: Boolean }

terminalHandler :: forall r. LoggerConfig -> Log ~> Run (EFFECT + r)
terminalHandler conf = case _ of
  Log level msg next -> do
    when (level >= conf.minLevel) do
      let
        printed = Dodo.print Ansi.ansiGraphics Dodo.twoSpaces $ case level of
          Debug -> foreground Ansi.Blue msg
          Info -> Dodo.text "[INFO]" <> msg
          Warn -> foreground Ansi.Yellow (Dodo.text "[WARN]") <> msg
          Error -> foreground Ansi.Red (Dodo.text "[ERROR]") <> msg

      liftEffect $ Console.log printed
    pure next

log :: forall r a. Loggable a => LogLevel -> a -> Run (LOG + r) Unit
log level msg = Run.lift _log $ Log level (toLog msg) unit

debug :: forall r a. Loggable a => a -> Run (LOG + r) Unit
debug = log Debug

info :: forall r a. Loggable a => a -> Run (LOG + r) Unit
info = log Info

warn :: forall r a. Loggable a => a -> Run (LOG + r) Unit
warn = log Warn

error :: forall r a. Loggable a => a -> Run (LOG + r) Unit
error = log Error

red :: String -> Doc GraphicsParam
red msg = foreground Ansi.Red (Dodo.text msg)

green :: String -> Doc GraphicsParam
green msg = foreground Ansi.Green (Dodo.text msg)

blue :: String -> Doc GraphicsParam
blue msg = foreground Ansi.Blue (Dodo.text msg)

yellow :: String -> Doc GraphicsParam
yellow msg = foreground Ansi.Yellow (Dodo.text msg)

cyan :: String -> Doc GraphicsParam
cyan msg = foreground Ansi.Cyan (Dodo.text msg)

magenta :: String -> Doc GraphicsParam
magenta msg = foreground Ansi.Magenta (Dodo.text msg)

logAndThrow :: forall r a. String -> Run (LOG + EFFECT + r) a
logAndThrow msg = error msg *> liftEffect (Exn.throw msg)