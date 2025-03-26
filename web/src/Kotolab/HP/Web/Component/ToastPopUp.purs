module Kotolab.HP.Web.Component.ToastPopUp where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Fmt as Fmt
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (useQuery, useState)
import Halogen.Hooks as Hooks

data Phase = Hidden | PopUp | Shown | FadeOut

derive instance Eq Phase

data Query a = Display a

make :: forall i o m. MonadAff m => H.Component Query i o m
make = Hooks.component \{ queryToken } _ -> Hooks.do
  value /\ valueId <- useState Hidden

  useQuery queryToken case _ of
    Display next -> do
      whenM ((_ == Hidden) <$> Hooks.get valueId) do
        Hooks.put valueId PopUp
        liftAff $ delay (Milliseconds 150.0)
        Hooks.put valueId Shown
        liftAff $ delay (Milliseconds 700.0)
        Hooks.put valueId FadeOut
        liftAff $ delay (Milliseconds 500.0)
        Hooks.put valueId Hidden

      pure (Just next)

  let
    ctx =
      { value
      }
  Hooks.pure (render ctx)
  where
  render ctx = do
    HH.div
      []
      [ HH.div
          [ HP.class_ $ ClassName
              "pointer-events-none overflow-hidden bottom-full absolute \
              \ left-1/2 -translate-x-1/2 z-40 bg-black/0 w-24 h-12"
          ]
          [ HH.div
              [ HP.class_ $ ClassName $
                  Fmt.fmt
                    @"absolute  left-1/2 -translate-x-1/2 \
                    \ p-2 bg-pink-700 text-xs text-pink-50 font-sans \
                    \ font-bold rounded shadow-lg transition-all ease-in-out \
                    \ {translate} {duration} {visibility}"
                    { duration: case ctx.value of
                        PopUp -> "duration-150"
                        FadeOut -> "duration-500"
                        _ -> ""
                    , visibility: case ctx.value of
                        Hidden -> "opacity-0 translate-y-full"
                        FadeOut -> "opacity-0"
                        _ -> "opacity-100"
                    , translate: case ctx.value of
                        Hidden -> "translate-y-full"
                        _ -> "translate-y-0"
                    }
              ]
              [ HH.text "Copied!"
              , HH.div
                  [ HP.class_ $ ClassName $
                      Fmt.fmt
                        @"absolute top-full left-1/2 -translate-x-1/2 w-0 h-0 \
                        \ border-l-8 border-r-8 border-t-8 border-l-transparent \
                        \ border-r-transparent border-t-pink-700"
                        {}
                  ]
                  []
              ]
          ]
      ]

-- attach
--   :: forall slot sl sa w i m _1
--    . IsSymbol slot
--   => Row.Cons slot (H.Slot _ _ _) _1 sl
--   => Proxy slot
--   -> sa
--   -> HH.HTML w i
--   -> HH.HTML (H.ComponentSlot sl m (Hooks.HookM m Unit)) (Hooks.HookM m Unit)
-- attach sl sa target = do
--   HH.div []
--     [ target
--     , HH.slot_ (Proxy :: _ slot) sa make {}
--     ]