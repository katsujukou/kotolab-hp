module Test.Foreign.Dayjs where

import Prelude

import Data.Enum (toEnum)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Foreign.Dayjs as Dayjs
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

spec :: Spec Unit
spec = describe "Foreign.Dayjs" do
  it "should be equal to itself" do
    d <- liftEffect $ Dayjs.now
    d `shouldEqual` d

  it "should success to parse" do
    let
      mbDay = Dayjs.parse "2025-03-26"
    mbDay `shouldNotEqual` Nothing
    (mbDay >>= Dayjs.year) `shouldEqual` toEnum 2025
    (mbDay >>= Dayjs.month) `shouldEqual` toEnum 3
    (mbDay >>= Dayjs.date) `shouldEqual` toEnum 26

  it "should set year" do
    let
      mbDay1 = Dayjs.parse "2025-03-26"
      mbDay2 = do
        d <- mbDay1
        y <- toEnum 2000

        pure $ Dayjs.setYear y d

    (mbDay2 >>= Dayjs.year) `shouldEqual` toEnum 2000
    (mbDay1 >>= Dayjs.year) `shouldEqual` toEnum 2025
    (mbDay2 >>= Dayjs.month) `shouldEqual` (mbDay1 >>= Dayjs.month)
    (mbDay2 >>= Dayjs.date) `shouldEqual` (mbDay1 >>= Dayjs.date)