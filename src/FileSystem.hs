{-# LANGUAGE FlexibleContexts #-}

module FileSystem
  ( filestat
  , create
  , open
  , close
  , read
  , write
  , link
  , unlink
  , truncate
  , mkdir
  , rmdir
  , symlink
  , FSError (..)
  )
where

import           Control.Monad.State
import           Control.Monad.Except
import           Data.Bit
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe
import qualified Data.Vector.Unboxed   as V
import           Data.Word
import           FileSystem.DataTypes
import           FileSystem.Internal
import           FileSystem.State
import           Prelude               hiding (read, truncate)
import           System.FilePath.Posix

-- | Returns information about file at 'FilePath'
filestat :: FS m => FilePath -> m FileStat
filestat path = do
  FSS { .. } <- get
  fileStat <$> (snd <$> getINOByPath path inodes mem)

-- TODO: limit the path lenght
-- | Creates new file at prefix 'FilePath' with name postfix 'FilePath'
create :: FS m => FilePath -> m ()
create path = do
  st@FSS { .. } <- get

  catchError (void $ getINOByPath path inodes mem) \_ -> do
    let index = fromMaybe (error $ show ENOSPC) (bitIndex 0 $ unIbm inodeBitMap)

    -- allocating mem and writing a filename to the first block
    let (newBlocks, newBMap) = findNFreeBlocks blockBitMap 1
        newIndex = fromIntegral $ head newBlocks
        newName = putName (BS.pack path)
                  $ fromIntegral $ blockSize metadata

    let newMem = init $ take newIndex mem
              <> pure (Block newName)
              <> drop newIndex mem

    -- adding record to the dir
    let ino = inodes !! index
        ni = ino { blockCount = 1
                  , linkCount = 1
                  , fileStat = FS (fromIntegral $ length path) File
                  , blocks = newBlocks
                  }

    put $ st { inodeBitMap = Ibm . V.modify (`flipBit` index) $ unIbm inodeBitMap
              , blockBitMap = newBMap
                -- it shouldn't panic at index 0,
                -- because 0th index is "/" root dir
                -- which shouldn't be deleted
                -- and something created over it
              , inodes = init $ take index inodes
                      <> pure ni
                      <> drop index inodes
              , mem = newMem
              }

    -- updating dir cont*nt
    let dirPath = dropFileName path
    INode { blocks } <- (inodes !!) <$> (fst <$> getINOByPath dirPath inodes mem)

    dir <- readDir blocks
    newDir <- writeDir (dir <> BS.pack ("\n" <> path <> ";" <> show index)) blocks

    put $ st { mem = newDir }
  throwError $ error $ show EEXIST


-- | Opens file at 'FilePath' returning 'FileDescriptor' for future uses
open :: FS m => FilePath -> m FileDescriptor
open path = do
  st@FSS { .. } <- get
  fd <- FileDescriptor (length fdlist) . blocks . snd
        <$> getINOByPath path inodes mem 
  put $ st { fdlist=fdlist <> [fd] }
  return fd

-- | Closes given 'FileDescriptor'
close :: FS m => FileDescriptor -> m ()
close FileDescriptor { unID } = do
  st@FSS { fdlist } <- get
  put $ st { fdlist = filter (\(FileDescriptor fid _ ) -> unID /= fid) fdlist }

-- | Reads 'Int' bytes at 'Int' offset from 'FileDescriptor'
read :: FS m => FileDescriptor -> Int -> Int -> m (V.Vector Word8)
read FileDescriptor { unBlocks } bytes off =
  V.take bytes . V.drop off <$> fromIndToVec unBlocks


-- | Writes 'Int' bytes at 'Int' offset to 'Filedescriptor'
write :: FS m => FileDescriptor -> Int -> Int -> V.Vector Word8 -> m Int
write FileDescriptor { unBlocks } bytes off d = do
  st@FSS { metadata=SBlock { blockSize }, mem} <- get

  fdBlocks <- fromIndToVec unBlocks
  when (off + bytes < V.length fdBlocks) (error $ show ENOMEM)

  put $ st { mem = writeMem (fromIntegral blockSize) mem
                   unBlocks (V.take bytes d) (V.length d)
           }
  return bytes

-- | Creates hard link in 'FilePath' to 'FilePath'
link :: FS m => FilePath -> FilePath -> m ()
link src dst = do
  st@FSS { inodes, mem } <- get
  index <- fst <$> getINOByPath src inodes mem

  let ino@INode { fileStat=FS{ fileType }
                , linkCount
                } = inodes !! index

  -- TODO: how to rewrite with when? probably create
  -- another function
  -- checking if src is not a dir
  let _ = case fileType of
            None      -> error $ show ENXIST
            Directory -> error $ show EISDIR
            _         -> False

  let dirPath = dropFileName src
  dirIndex <- fst <$> getINOByPath dirPath inodes mem

  let INode { blocks } = inodes !! dirIndex
  indexDst <- fst <$> getINOByPath dst inodes mem

  -- updating dir cont*nt
  dir <- readDir blocks
  newDir <- writeDir (dir <> BS.pack ("\n" <> dst <> ";" <> show indexDst)) blocks

  put $ st { inodes = init $ take index inodes
                     <> pure (ino { linkCount = linkCount + 1 })
                     <> drop index inodes
           , mem = newDir
           }

-- | Destroys link at 'FilePath'
unlink :: FS m => FilePath -> m ()
unlink path = do
  st@FSS { .. } <- get

  lc <- linkCount . (inodes !!) . fst <$> getINOByPath path inodes mem

  -- TODO: unlink should exit if this `if` statement is correct
  when (lc > 1) $ rmEntryFromDir path $ dropFileName path

  (ni, nim, nb) <- unlinkINodeByPath path inodes inodeBitMap blockBitMap
  put st { inodes = ni
         , inodeBitMap = nim
         , blockBitMap = nb
         }

-- | Truncates file at 'FilePath' to 'Int' bytes
truncate :: FS m => FilePath -> Int -> m ()
truncate path size = do
  st@FSS { metadata=SBlock{ blockSize }, inodes, mem, blockBitMap} <- get

  (index, ino@INode { blockCount }) <- getINOByPath path inodes mem
  let (newBlocks, newBitMap) = findNFreeBlocks
                               blockBitMap
                               (size - fromIntegral (blockCount * blockSize))

  -- if size > size of blocks of inodes
      newInos = init $ take index inodes
             <> pure (ino { blocks=newBlocks })
             <> drop index inodes

  put st { inodes=newInos, blockBitMap=newBitMap }


-- | Creates directory at 'FilePath'
mkdir :: FS m => FilePath -> m ()
mkdir path = do
  st@FSS { .. } <- get

  -- TODO: how to check it smoothly
  _ <- getINOByPath path inodes mem 

  let index = case bitIndex 0 $ unIbm inodeBitMap of
            Just i  -> i
            Nothing -> error $ show ENOSPC


  let (newBlocks, newBMap) = findNFreeBlocks blockBitMap 1
      newIndex = fromIntegral $ head newBlocks
      newName = putName (BS.pack path)
                $ fromIntegral $ blockSize metadata

  let newMem = init $ take newIndex mem
            <> pure (Block newName)
            <> drop newIndex mem


  let ino = inodes !! index
  let ni = ino { blockCount = 1
               , linkCount = 1
               , fileStat = FS (fromIntegral $ length path) File
               , blocks = newBlocks
               }

  -- TODO: update directory cont*nt with this file
  put $ st { inodeBitMap = Ibm . V.modify (`flipBit` index) $ unIbm inodeBitMap
           , blockBitMap = newBMap
           , inodes = init $ take index inodes
                   <> pure ni
                   <> drop index inodes
           , mem = newMem
           }

-- | Removes directory at 'FilePath'
rmdir :: FS m => FilePath -> m ()
rmdir path = do
  FSS { inodes, mem } <- get

  blk <-  blocks . snd <$> getINOByPath path inodes mem
  dirBlob <- readDir blk
  dirLinks <- readDirLinks dirBlob
  mapM_ unlink dirLinks

  unlink $ dropFileName path

-- | Creates symbolic link from 'FilePath' to 'FilePath'
symlink :: FS m => FilePath -> FilePath -> m ()
symlink src dst = do
  -- try to create file and catch error in this case
  let v = toVectorWord8 $ BS.pack dst
  -- changing size of src file to the size of ther dst link
  truncate src $ V.length v

  fd <- open src
  _ <- write fd (V.length v) 0 v
  close fd