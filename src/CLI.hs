module CLI
  ( getArgs,
  )
where

import Options.Generic
import System.IO
import System.Directory
import System.FilePath

newtype CMD = CMD
  { mountDir :: Maybe FilePath <?> "Dir with filesystems(files) to mount, \
                                   \defaults to current dir"
  }
  deriving (Generic, Show)

instance ParseRecord CMD

getArgs :: IO CMD
getArgs = do
  getRecord "FileSystem model"

-- | Get all FileSystem Model files at 'FilePath'
getFSFiles :: FilePath -> IO [FilePath]
getFSFiles path = do
  filter ((== "fsm") . takeExtension) <$> listDirectory path
