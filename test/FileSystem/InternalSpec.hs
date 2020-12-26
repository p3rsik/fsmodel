module FileSystem.InternalSpec (spec) where

import           Control.Exception
import           Control.Monad.State
import           Control.Monad.Except
import           Data.Bit
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed   as V
import           FileSystem
import           FileSystem.DataTypes
import           FileSystem.Internal
import           FileSystem.State
import           Test.Hspec


newtype App a = App
  { runApp :: StateT FState (ExceptT FSError IO) a
  } deriving (Monad, Functor, Applicative, MonadState FState, MonadError FSError, MonadIO)

eval :: FState -> App a -> IO a
eval s a = either (\e -> error $ "Failed with: " <> show e) return =<< runExceptT (evalStateT (runApp a) s)
run :: FState -> App a -> IO (a, FState)
run s a = either (\e -> error $ "Failed with: " <> show e) return =<< runExceptT (runStateT (runApp a) s)

spec :: Spec
spec = do
  describe "getName" $ do
    it "returns a file name from memory" $ do
      let memory = mem $ createFS 16 16 16
      (trimNull . getName) memory `shouldBe` "/"

    it "panics due to the empty list" $ do
      (trimNull . getName) [] `shouldBe` []
  
  describe "getINOByPath" $ do
    it "return (index, INode)" $ do
      let fs = createFS 16 16 16
          expect = (0, head $ inodes fs)
      got <- eval fs $ getINOByPath "/" (inodes fs) (mem fs)
      expect `shouldBe` got

    it "throws error; empty inodes list" $ do
      let fs = createFS 16 16 16
      (`shouldThrow` anyException) <$> eval fs
        $ getINOByPath "/" [] (mem fs)

    it "throws error; empty mem list" $ do
      let fs = createFS 16 16 16
      (`shouldThrow` anyException) <$> eval fs
        $ getINOByPath "/" (inodes fs) []

  describe "fromVecToBlock" $ do
    it "return list with one block" $ do
      let vec = V.fromList $ B.unpack $ BS.pack "test sentence"
          got = fromVecToBlock vec 16
          expected = Block . V.fromList . B.unpack . BS.pack
                 <$> ["test sentence\NUL\NUL\NUL"]
      got `shouldBe` expected

    it "return many blocks list" $ do
      let vec = V.fromList $ B.unpack $ BS.pack "long sentence"
          got = fromVecToBlock vec 4
          expected = Block . V.fromList . B.unpack . BS.pack
                 <$> ["long", " sen", "tenc", "e\NUL\NUL\NUL"]
      got `shouldBe` expected

    it "return empty list" $ do
      let vec = V.fromList $ B.unpack $ BS.pack ""
          got = fromVecToBlock vec 4
          expected = []
      got `shouldBe` expected

  describe "findNFreeBlocks" $ do
    it "return ([Index Block], BlockBitMap)" $ do
      let fs = createFS 16 16 16
          expect = ([1..5], foldl f (blockBitMap fs) [1..5])
          got = findNFreeBlocks (blockBitMap fs) 5
      got `shouldBe` expect

    it "return ([Index Block], BlockBitMap) with segmentation" $ do
      let fs = createFS 16 16 16
          expect = ([1..5] <> [7], foldl f (blockBitMap fs) [1..7])
          got = findNFreeBlocks (f (blockBitMap fs) 6) 6
      got `shouldBe` expect

      where f :: BlockBitMap -> Int -> BlockBitMap
            f fsys index = Bbm . V.modify (`flipBit` index) . unBbm $ fsys







