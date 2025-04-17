module Kotolab.HP.API.Effect.Hackbar where

import Prelude

import Cloudflare.KV (KVNamespace)
import Data.Codec (decode)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Date (Month, Year)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..))
import Fmt as Fmt
import Kotolab.HP.API.Schema.Json as JSON
import Kotolab.HP.API.Schema.Types as SchemaTypes
import Kotolab.HP.Backend.Cloudflare.KV as KV
import Kotolab.HP.Backend.Effects.Log (LOG)
import Kotolab.HP.Backend.Error (BackendError(..))
import Run (AFF, EFFECT, Run, liftAff, on, send)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

data HackbarF a
  = GetHackbarAttendList Year Month (Array SchemaTypes.HackbarAttendInfo -> a)
  | UpdateHackbarAttendList Year Month (Array SchemaTypes.HackbarAttendInfo) (String -> a)

derive instance Functor HackbarF

type HACKBAR r = (hackbar :: HackbarF | r)

_hackbar :: Proxy "hackbar"
_hackbar = Proxy

interpret :: forall r a. (HackbarF ~> Run r) -> Run (HACKBAR + r) a -> Run r a
interpret handler = Run.interpret (on _hackbar handler send)

cloudflareKVHandler
  :: forall r
   . KVNamespace
  -> HackbarF ~> Run (EXCEPT BackendError + LOG + AFF + EFFECT + r)
cloudflareKVHandler kv = case _ of
  GetHackbarAttendList y m reply -> do
    let key = Fmt.fmt @"{y}/{m}" { y: fromEnum y, m: fromEnum m }
    json <- liftAff $ KV.getJson kv key
    case decode (CAC.maybe $ CA.array SchemaTypes.hackbarAttendInfo) json of
      Right mbAttendList
        | Just attendList <- mbAttendList -> pure $ reply $ attendList
        | otherwise -> Except.throw $ HackbarAttendInfoNotFound y m
      Left err -> Except.throw $ FailedToDecodeHackbarAttendInfo err
  UpdateHackbarAttendList y m attendList reply -> do
    let
      key = Fmt.fmt @"{y}/{m}" { y: fromEnum y, m: fromEnum m }
      document = JSON.stringify (CA.array SchemaTypes.hackbarAttendInfo) attendList
    liftAff $ KV.put kv key document
    pure $ reply document

getHackbarAttendList :: forall r. Year -> Month -> Run (HACKBAR + r) (Array SchemaTypes.HackbarAttendInfo)
getHackbarAttendList y m = Run.lift _hackbar $ GetHackbarAttendList y m identity

updateHackbarAttendList :: forall r. Year -> Month -> Array SchemaTypes.HackbarAttendInfo -> Run (HACKBAR + r) String
updateHackbarAttendList y m attendList = Run.lift _hackbar $ UpdateHackbarAttendList y m attendList identity
