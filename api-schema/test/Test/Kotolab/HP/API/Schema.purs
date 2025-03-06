module Test.Kotolab.HP.API.Schema where

import Prelude

import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Kotolab.HP.API.Schema" do
    it "should run some tests" do
      42 `shouldEqual` (40 + 2)