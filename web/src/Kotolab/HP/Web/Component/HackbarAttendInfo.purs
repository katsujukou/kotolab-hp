module Kotolab.HP.Web.Component.HackbarAttendInfo where

import Prelude

import Data.Array (fold)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Fmt as Fmt
import Foreign.Dayjs as Dayjs
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (captures, useLifecycleEffect, useState, useTickEffect)
import Halogen.Hooks as Hooks
import Kotolab.HP.Web.Assets (assets, fromAssetURL)
import Kotolab.HP.Web.Component.Types (HackbarAttendInfo)

type Input =
  { attendList :: Array HackbarAttendInfo
  }

make :: forall q o m. MonadEffect m => H.Component q Input o m
make = Hooks.component \_ inps -> Hooks.do
  attendList /\ attendListId <- useState []

  useInputAttendList { attendListId } { attendList: inps.attendList }

  useLifecycleEffect do
    Console.logShow $ inps
    pure Nothing
  let
    ctx =
      { attendList
      }

  Hooks.pure (render ctx)
  where

  useInputAttendList nonDeps deps@{ attendList } = captures deps useTickEffect do
    Hooks.put nonDeps.attendListId attendList
    pure Nothing

  render ctx = do
    HH.div
      [ HP.class_ $ ClassName "relative w-72 sm:w-120 p-4 border rounded-lg border-dashed border-red-300 flex flex-col items-center" ]
      [ HH.h3
          [ HP.class_ $ ClassName "font-retro text-pink-700 my-2" ]
          [ HH.text "🍸️HACK.BAR 出勤情報🍸️" ]

      , HH.div
          [ HP.class_ $ ClassName "my-4 grid grid-cols-8 font-josefin-sans text-gray-700" ] $ fold $
          ctx.attendList <#> \attend ->
            do
              [ HH.span [ HP.class_ $ ClassName "col-span-2 mr-1 text-right " ]
                  [ HH.text $ Dayjs.format "MM/DD" attend.date
                  ]
              , HH.span [ HP.class_ $ ClassName "col-span-2" ]
                  [ HH.text "("
                  , HH.span
                      [ HP.class_ $ ClassName $ Fmt.fmt @"{color}"
                          { color: case Dayjs.day attend.date of
                              Just Dayjs.Sun -> "text-red-600"
                              Just Dayjs.Sat -> "text-blue-600"
                              _ -> ""
                          }
                      ]
                      [ HH.text $ maybe "-" show $ Dayjs.day attend.date
                      ]
                  , HH.text ")"
                  ]
              , HH.span [ HP.class_ $ ClassName "col-span-1" ] [ HH.text "..." ]
              , HH.span [ HP.class_ $ ClassName "col-span-3" ]
                  [ HH.text $ Fmt.fmt @"{from} - {to}"
                      { from: show attend.startTime, to: show attend.endTime }
                  ]
              ]

      , HH.div [ HP.class_ $ ClassName "font-retro text-sm text-gray-700 flex flex-col justify-start" ]
          [ HH.div [ HP.class_ $ ClassName "font-retro text-xs text-gray-600" ]
              [ HH.text "18:00 Open - 23:00 Close" ]
          , HH.div []
              [ HH.text "定休日：毎週 月・火曜日" ]
          ]
      , HH.img
          [ HP.src $ fromAssetURL assets.images.roseFrameAngle
          , HP.class_ $ ClassName
              "absolute top-0 left-0 sm:w-1/3 w-1/2 \
              \sm:-translate-y-[12px] -translate-y-[8px] sm:-translate-x-[12px] -translate-x-[12px] \
              \ pointer-events-none brightness-85 saturate-200"
          ]
      , HH.img
          [ HP.src $ fromAssetURL assets.images.roseFrameAngle
          , HP.class_ $ ClassName
              "absolute bottom-0 right-0 sm:w-1/3 w-1/2 rotate-180 \
              \sm:translate-y-[16px] translate-y-[8px] sm:translate-x-[12px] translate-x-[12px] \
              \ pointer-events-none brightness-85 saturate-200"
          ]
      ]
