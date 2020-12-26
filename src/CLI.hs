{-# LANGUAGE FlexibleContexts #-}

module CLI
  ( getArgs
  , runFs
  )
where

import           CLI.Internal
import           Options.Generic

newtype CMD = CMD
  { mountDir ::
      Maybe FilePath
        <?> "Dir with filesystems(files) to mount, \
            \defaults to current dir"
  }
  deriving (Generic, Show)

instance ParseRecord CMD

getArgs :: IO FilePath
getArgs = do
  p <- getRecord "FileSystem model"
  case p of
    Just p' -> return p'
    Nothing -> return "."


runFs :: FilePath -> IO ()
runFs fp = do
  help
  loop fp
