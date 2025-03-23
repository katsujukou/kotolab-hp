module Kotolab.HP.Web.Component.SidebarToggleButton where

import Prelude hiding (flip)

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Fmt as Fmt
import Halogen (ClassName(..), liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (useState)
import Halogen.Hooks as Hooks
import Kotolab.HP.Web.Assets (assets, fromAssetURL)

data ButtonState = Hidden | Display

derive instance Eq ButtonState
derive instance Generic ButtonState _
instance Show ButtonState where
  show = genericShow

flip :: ButtonState -> ButtonState
flip Hidden = Display
flip Display = Hidden

shouldDisplay :: ButtonState -> Boolean
shouldDisplay Display = true
shouldDisplay _ = false

data Output
  = Clicked
  | ToggleDone ButtonState

type Input = { value :: Boolean }

eor :: Boolean -> Boolean -> Boolean
eor true false = true
eor false true = true
eor _ _ = false

infix 5 eor as ^

make :: forall q m. MonadAff m => H.Component q Input Output m
make = Hooks.component \{ outputToken } inps -> Hooks.do
  btnState /\ btnStateId <- useState (if inps.value then Display else Hidden)
  rotating /\ rotatingId <- useState false

  useInputValueEffect { btnStateId, rotatingId } { value: inps.value }

  let
    handleToggle = do
      unlessM (Hooks.get rotatingId) do
        Hooks.raise outputToken Clicked
        s0 <- Hooks.get btnStateId
        Hooks.put rotatingId true
        liftAff $ Aff.delay (Milliseconds 400.0)
        let s1 = flip s0
        Hooks.put btnStateId s1
        Hooks.put rotatingId false
        Hooks.raise outputToken $ ToggleDone s1

    ctx =
      { handleToggle
      , btnState
      , rotating
      }

  Hooks.pure (render ctx)
  where
  useInputValueEffect { btnStateId, rotatingId } deps@{ value } = Hooks.captures deps Hooks.useTickEffect do
    cur <- Hooks.get btnStateId
    when (cur == Hidden && value == true) do
      Hooks.put rotatingId true
      liftAff $ Aff.delay (Milliseconds 400.0)
      Hooks.put rotatingId false
      Hooks.put btnStateId Display

    pure Nothing

  render ctx = do
    let
      btnCls =
        Fmt.fmt
          @"fixed top-3 right-3 \
          \ flex justify-center items-center bg-[rgba(255,255,255,0.5)] rounded-full \
          \ transition-all duration-400 rotate-{rotate}\
          \ opacity-{opacity} {pointer_event}"
          { rotate: if ctx.rotating ^ (shouldDisplay ctx.btnState) then "0" else "180"
          , opacity: if ctx.rotating ^ (shouldDisplay ctx.btnState) then "100" else "0"
          , pointer_event: if shouldDisplay ctx.btnState then "" else "pointer-events-none"
          }

    do
      HH.button
        [ HP.class_ $ ClassName btnCls
        , HE.onClick \_ -> ctx.handleToggle
        ]
        [ HH.span [ HP.class_ $ ClassName "fixed font-josefin-sans text-pink-900 font-light text-xs" ]
            [ HH.text "MENU" ]
        , HH.img
            [ HP.src $ fromAssetURL assets.images.roseFrame
            , HP.class_ $ ClassName "top-0 left-0 w-[64px] h-[64px]"
            ]
        ]