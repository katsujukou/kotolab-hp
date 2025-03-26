module Test.Foreign.Main where

import Prelude

import Effect (Effect)
import Test.Foreign.Dayjs as Dayjs
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Dayjs.spec