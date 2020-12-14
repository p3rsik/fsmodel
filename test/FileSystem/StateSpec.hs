module FileSystem.StateSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import FileSystem.State

spec :: Spec
spec = describe "loadFS and unloadFS roundabout" $ do
  it "loadFS . unloadFS fs == fs" $ do
    property $ \bSize bCount iCount -> 
      let fs = createFS bSize bCount iCount in
        loadFS (unloadFS fs) `shouldBe` Just fs
  it "unloadFS $ loadFS fs == fs" $ do
    pending
