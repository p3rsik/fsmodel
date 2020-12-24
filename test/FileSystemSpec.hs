module FileSystemSpec (spec) where

import           Control.Exception
import           Control.Monad.State
import           Control.Monad.Except
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed   as V
import           Data.Word
import           FileSystem
import           FileSystem.DataTypes
import           FileSystem.Internal
import           FileSystem.State
import           Prelude               hiding (read, truncate)
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
  describe "filestat" $ do
    it "check root (/) directory" $ do
      let fs = createFS 16 16 16
      -- print fs
      let res = FS 16 Directory
      res' <- eval fs $ filestat "/"
      res' `shouldBe` res

    it "check /kek file" $ do
      let fs = createFS 16 16 16
      let res = FS 16 File
      res' <- eval fs $ do
        create "/kek"
        filestat "/kek"
      res' `shouldBe` res
    
  describe "create" $ do
    it "create new file" $ do
      let fs = createFS 16 16 16
      got <- eval fs $ do
        create "/kek"
        filestat "/kek"
      let expected = FS 16 File
      got `shouldBe` expected

    it "throws error EEXIST" $ do
      let fs = createFS 16 16 16
      (`shouldThrow` anyException) <$> eval fs $ create "/"

  describe "open" $ do
    it "open file" $ do
      let fs = createFS 16 16 16
      let res = FileDescriptor 0 [1]
      (fd, nfs) <- run fs $ do
        create "/kek"
        open "/kek"
      fd `shouldBe` res
      fdlist nfs `shouldBe` [res]

  describe "close" $ do
    it "close file" $ do
      let fs = createFS 16 16 16
      (_, nfs) <- run fs $ do
        create "/kek"
        fd <- open "/kek"
        close fd
      fdlist nfs `shouldBe` []

  describe "read" $ do
    it "read from file" $ do
      let fs = createFS 16 16 16
          expected = toVectorWord8 $ BS.pack "lol"
      (_, nfs) <- run fs $ do
        create "/kek"
        truncate "/kek" 64
        open "/kek"
        -- write fd 3 0 expected

      print nfs

      got <- eval nfs $ do
        fd <- open "/kek"
        read fd 3 0

      print nfs
      got `shouldBe` expected
        
        
      
    -- write,
    -- link,
    -- unlink,
  describe "truncate" $ do
    it "increase dir size" $ do
      let fs = createFS 16 16 16
          expected = FS 8 Directory
      (got, _) <- run fs $ do
        truncate "/" 117
        filestat "/"
      got `shouldBe` expected

    it "increase file size" $ do
      let fs = createFS 16 16 16
          expected = FS 4 File
      (got, _) <- run fs $ do
        create "/kek"
        truncate "/kek" 64
        filestat "/kek"
      got `shouldBe` expected

    
    -- mkdir,
    -- rmdir,
    -- symlink,