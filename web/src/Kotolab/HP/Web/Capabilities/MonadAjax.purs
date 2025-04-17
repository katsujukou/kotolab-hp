module Kotolab.HP.Web.Capabilities.MonadAjax where

import Prelude

import Data.HTTP.Method (Method)
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
import Data.String.CaseInsensitive (CaseInsensitiveString)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (HalogenM, lift)
import Halogen.Hooks (HookM)

type URL = String

newtype Header = Header (Tuple CaseInsensitiveString (Array String))

header :: String -> String -> Header
header k v = Header $ Tuple (wrap k) [ v ]

infix 5 header as :

headerMany :: String -> Array String -> Header
headerMany k vs = Header $ Tuple (wrap k) vs

infix 5 headerMany as :*

type Headers = Array Header

class MonadAff m <= MonadAjax m where
  sendRequest :: Method -> URL -> Maybe String -> Headers -> m { statusCode :: Int, body :: String }

instance monadAjaxHalogenM ::
  MonadAjax m =>
  MonadAjax (HalogenM st act slo o m)
  where
  sendRequest method url body headers = lift $ sendRequest method url body headers

instance monadAjaxHookM ::
  MonadAjax m =>
  MonadAjax (HookM m)
  where
  sendRequest method url body headers = lift $ sendRequest method url body headers