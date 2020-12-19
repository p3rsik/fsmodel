{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module FileSystem.SerializeSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import FileSystem.Serialize
import FileSystem.DataTypes
import FileSystem.State
import Data.Word
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString as B
import Data.Bit

instance Arbitrary FileType where
  arbitrary = do
    x <- choose (0, 3)
    return $ toEnum x

spec :: Spec
spec = do
  describe "Serialize and deserialize Word64" $ do
    it "decode . encode" $ do
      property $ \(w :: Word64) -> do
        decode (encode w) `shouldBe ` Right w
  describe "Serialize and deserialize Word8" $ do
    it "decode . encode" $ do
      property $ \(w :: Word8) -> do
        decode (encode w) `shouldBe ` Right w
  describe "Serialize and deserialize SuperBlock" $ do
    it "decode . encode" $ do
      property $ \bs bc fb ic fi -> do
        let sb = SBlock bs bc fb ic fi
        decode (encode sb) `shouldBe` Right sb
  describe "Serialize and deserialize BlockBitMap" $ do
    it "decode . encode" $ do
      property $ \b -> do
        let b' = Bbm . castFromWords8 $ V.fromList b
        decode (encode b') `shouldBe` Right b'
  describe "Serialize and deserialize INodeBitMap" $ do
    it "decode . encode" $ do
      property $ \b -> do
        let b' = Ibm . castFromWords8 $ V.fromList b
        decode (encode b') `shouldBe` Right b'
  describe "Serialize and deserialize FileType" $ do
    it "decode . encode" $ do
      property $ \(ft :: FileType) -> do
        decode (encode ft) `shouldBe` Right ft
  describe "Serialize and deserialize FileStat" $ do
    it "decode . encode" $ do
      property $ \ft size -> do
        let fs = FS size ft
        decode (encode fs) `shouldBe` Right fs
  describe "Serialize and deserialize INode" $ do
    it "decode . encode" $ do
      property $ \bc lc ft size -> do
        let fs = FS size ft
        let bc' = if bc > 16 then 16 else bc
        let lc' = if lc > 16 then 16 else lc
        let node = INode bc' lc' fs $ replicate (fromIntegral bc') bc
        decode (encode node) `shouldBe` Right node
  describe "Serialize and deserialize Block" $ do
    it "decode . encode" $ do
      property $ \m' -> do
        let m = V.fromList m'
        decodeBlock (V.length m) (encodeBlock $ Block m) `shouldBe` Right (Block m)
  describe "Serialize and deserialize FState" $ do
    it "decode . encode" $ do
      property $ do
        let fs = createFS 8 8 8
        decode (encode fs) `shouldBe` Right fs
