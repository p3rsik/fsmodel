module FileSystem.StateSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import FileSystem.State
import Data.Bits
import Data.Word

spec :: Spec
spec = describe "bytesToWord64" $ do
  it "Should give `shift 1 63` for [shift (1 :: Word8) 7, 0, 0, 0, 0, 0, 0, 0]" $ do
    let res = shift (1 :: Word64) 63
    bytesToWord64 [128, 0, 0, 0, 0, 0, 0, 0] `shouldBe` Just res
  it "Should fail if list isn't 8-bytes long" $ do
    property $ \xs -> if length xs == 8
                      then return ()
                      else bytesToWord64 xs `shouldBe` Nothing
