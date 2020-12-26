module CLI.Internal
  ( St (..)
  , help
  , loop
  )
where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.ByteString      as B
import           Data.List
import           FileSystem
import           FileSystem.Serialize
import           FileSystem.State
import           System.Directory
import           System.FilePath
import           Text.Read            (readMaybe)
import           System.IO (hFlush, stdout)

-- | Top level app state
data St = St
  { currentPath :: FilePath
  , fstate      :: FState
  }

-- | Top level monad stack for running app
newtype App a = App
  { runApp :: StateT St IO a
  } deriving (Monad, Functor, Applicative, MonadState St, MonadIO)

eval :: St -> App a -> IO a
eval s a = evalStateT (runApp a) s

-- | Stack for filesystem
newtype FsApp a = FsApp
  { runFsApp :: StateT FState (ExceptT FSError IO) a
  } deriving (Monad, Functor, Applicative, MonadState FState, MonadError FSError, MonadIO)

run :: FState -> FsApp a -> IO (a, FState)
run s a = either (\e -> error $ "Failed with: " <> show e) return =<< runExceptT (runStateT (runFsApp a) s)

-- | Get all FileSystem Model files at 'FilePath'
getFSFiles :: FilePath -> IO [FilePath]
getFSFiles path = do
  filter ((== ".fs") . takeExtension) <$> listDirectory path

-- | ls is a simple imitation of unix shell command ls
ls :: (MonadState St m, MonadIO m) => Maybe FilePath -> m [FilePath]
ls (Just p) = liftIO $ listDirectory p
ls Nothing = do
  st <- get
  liftIO . listDirectory $ currentPath st

cd :: MonadState St m => FilePath -> m ()
cd p = do
  st <- get
  let p' = currentPath st
  case p of
    "."  -> return ()
    ".." -> put st {currentPath = takeDirectory p'}
    np   -> put st {currentPath = np}

flush :: IO ()
flush = hFlush stdout

loopFs :: (MonadState St m, MonadIO m) => m ()
loopFs = do
  St {..} <- get
  liftIO $ putStr $ currentPath <> ": "
  liftIO flush

  c <- liftIO getLine
  case c of
    "exit" -> return ()

  -- get new state and path after performing command
  (np, nfs) <- liftIO . run fstate $ command currentPath c
  let nst = St np nfs
  -- reset current
  put nst

  -- loop again for the next command
  loopFs
  where
    command :: FS m => FilePath -> String -> m FilePath
    command fp c
      | "filestat" `isSuffixOf` c = handleFileStat fp c
      | "create" `isSuffixOf` c = handleCreate fp c
      | "open" `isSuffixOf` c = handleOpen fp c
      | "close" `isSuffixOf` c = handleClose fp c
      | "read" `isSuffixOf` c = handleRead fp c
      | "write" `isSuffixOf` c = handleWrite fp c
      | "link" `isSuffixOf` c = handleLink fp c
      | "unlink" `isSuffixOf` c = handleUnlink fp c
      | "truncate" `isSuffixOf` c = handleTruncate fp c
      | "mkdir" `isSuffixOf` c = handleMkdir fp c
      | "rmdir" `isSuffixOf` c = handleRmdir fp c
      | "symlink" `isSuffixOf` c = handleSymlink fp c
      | otherwise = undefined
    handleFileStat :: FS m => FilePath -> String -> m FilePath
    handleFileStat = undefined
    handleCreate :: FS m => FilePath -> String -> m FilePath
    handleCreate = undefined
    handleOpen :: FS m => FilePath -> String -> m FilePath
    handleOpen = undefined
    handleClose :: FS m => FilePath -> String -> m FilePath
    handleClose = undefined
    handleRead :: FS m => FilePath -> String -> m FilePath
    handleRead = undefined
    handleWrite :: FS m => FilePath -> String -> m FilePath
    handleWrite = undefined
    handleLink :: FS m => FilePath -> String -> m FilePath
    handleLink = undefined
    handleUnlink :: FS m => FilePath -> String -> m FilePath
    handleUnlink = undefined
    handleTruncate :: FS m => FilePath -> String -> m FilePath
    handleTruncate = undefined
    handleMkdir :: FS m => FilePath -> String -> m FilePath
    handleMkdir = undefined
    handleRmdir :: FS m => FilePath -> String -> m FilePath
    handleRmdir = undefined
    handleSymlink :: FS m => FilePath -> String -> m FilePath
    handleSymlink = undefined

help :: IO ()
help = do
  putStrLn "Welcome to the FileSystem Model, please choose one of the options:"
  putStrLn "  'ls' - list available filesystem images"
  putStrLn "  'new [name] [block size] [block count] [inode count]' - create new filesystem with specified parameters"
  putStrLn "  'delete [name]' - delete existing filesystem"
  putStrLn "  'load [name]' - load existing filesystem"
  putStrLn "  'help' - to see this message"
  putStrLn "  'exit' - to exit"

loop :: FilePath -> IO ()
loop fp = do
  setCurrentDirectory fp
  curDir <- getCurrentDirectory
  putStr $ curDir <> ":> "
  liftIO flush

  input <- getLine

  command input
  where
    command :: String -> IO ()
    command "ls" = do
      con <- getFSFiles "."
      putStrLn $ foldl' (\ac el -> ac <> "\n" <> el) "" con
      curDir <- getCurrentDirectory
      loop curDir
    command "exit" = return ()
    command "help" = do 
        help
        curDir <- getCurrentDirectory
        loop curDir
    command s
      | "new" `isPrefixOf` s = handleNew s
      | "delete" `isPrefixOf` s = handleDelete s
      | "load" `isPrefixOf` s = handleLoad s
      | otherwise = do
        putStrLn $ "Can't recognize command: '" <> s <> "'\nPlease try again"
        curDir <- getCurrentDirectory
        loop curDir

    handleNew :: String -> IO ()
    handleNew s = do
      let c = words s
      unless (length c == 5) $ do
        putStrLn $ "'new' command accepts 4 arguments, but " <> show (length c - 1) <> " were given"
        curDir <- getCurrentDirectory
        loop curDir

      let t = readMaybe <$> drop 2 c
      when (Nothing `elem` t) $ do
        putStrLn "Can't convert one of the arguments, make sure that you supplied string for the name and 3 integers"
        curDir <- getCurrentDirectory
        loop curDir

      let [Just bSize, Just bCount, Just iCount] = t
          fs = createFS bSize bCount iCount
      curDir <- getCurrentDirectory
      let name = c !! 1 <> ".fs"
      B.writeFile (joinPath [curDir, name]) $ encode fs
      putStrLn $ "Created new filesystem with name " <> name
      curDir <- getCurrentDirectory
      loop curDir

    handleDelete :: String -> IO ()
    handleDelete s = do
      let c = words s
      unless (length c == 2) $ do
        putStrLn $ "'delete' command accepts 1 argument, but " <> show (length c - 1) <> " were given"
        curDir <- getCurrentDirectory
        loop curDir
      let [_, name] = c

      curDir <- getCurrentDirectory
      let fname = joinPath [curDir, name]
      ex <- doesFileExist fname
      unless ex $ do
        putStrLn $ "File system with name " <> name <> " does not exist"
        loop curDir

      removeFile fname
      loop curDir

    handleLoad :: String -> IO ()
    handleLoad s = do
      let c = words s
      unless (length c == 2) $ do
        putStrLn $ "'load' command accepts 1 arguments, but " <> show (length c - 1) <> " were given"

      let [_, name] = c
      curDir <- getCurrentDirectory
      let fname = joinPath [curDir, name]
      ex <- doesFileExist fname
      unless ex $ do
        putStrLn $ "File system with name " <> name <> " does not exist"
        loop curDir

      fsenc <- B.readFile fname
      case decode fsenc of
        Right fs -> do
          let st = St curDir fs
          eval st loopFs
        Left (DecodeEr e) -> do
          putStrLn $ "Can't load file system: " <> e
          loop curDir
