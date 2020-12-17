{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FileSystemSpec (spec) where

import FileSystem
import FileSystem.State
import Test.Hspec
import Control.Monad.State
import Control.Monad.Except
import FileSystem.Internal

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
      let res = FS 1 Directory
      res' <- eval fs $ filestat "/"
      res' `shouldBe` res

    it "check /kek file" $ do
      let fs = createFS 16 16 16
      let res = FS 1 File
      res' <- eval fs $ do
        create "/kek"
        filestat "/kek"
      res' `shouldBe` res

  describe "create" $ do
    it "create new file" $ do
      let fs = createFS 16 16 16
      res' <- eval fs $ do
        create "/kek"
        filestat "/kek"
      let res = FS 1 File
      res' `shouldBe` res

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
    -- read,
    -- write,
    -- link,
    -- unlink,
    -- truncate,
    -- mkdir,
    -- rmdir,
    -- symlink,
