module Kotolab.HP.Backend.Error where

import Prelude

import Data.Codec.Argonaut as CA
import Data.Date (Month, Year)
import Data.Enum (fromEnum)
import Dodo as Dodo
import Dodo.Ansi (Color(..))
import Dodo.Ansi as Ansi
import Fmt as Fmt
import HTTPurple (notFound)
import HTTPurple as HTTPurple
import Kotolab.HP.API.Effects.Log (class Loggable)

data BackendError
  = FailedToDecodeHackbarAttendInfo CA.JsonDecodeError
  | HackbarAttendInfoNotFound Year Month

instance Loggable BackendError where
  toLog = case _ of
    FailedToDecodeHackbarAttendInfo jsonDecodeError -> do
      (Ansi.foreground Red $ Dodo.text "HACKBAR出勤情報のデコードに失敗") Dodo.<%>
        Dodo.indent
          (Ansi.foreground Black $ Dodo.text $ CA.printJsonDecodeError jsonDecodeError)
    HackbarAttendInfoNotFound y m -> do
      Dodo.text $ Fmt.fmt @"{y}年{m}月度のHACKBAR出勤情報がありませんでした"
        { y: fromEnum y, m: fromEnum m }

toResponse :: BackendError -> HTTPurple.ResponseM
toResponse = case _ of
  HackbarAttendInfoNotFound _ _ -> notFound
  _ -> HTTPurple.internalServerError "予期せぬエラーが発生しました"