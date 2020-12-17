{-# LANGUAGE FlexibleContexts #-}

module FileSystem
  ( filestat,
    create,
    open,
    close,
    read,
    write,
    link,
    unlink,
    truncate,
    mkdir,
    rmdir,
    symlink,
    FSError (..)
  )
where

import           Control.Monad.State
import           Data.Bit
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Unboxed   as V
import           Data.Word
import           FileSystem.Internal
import           FileSystem.State
import           Prelude               hiding (read, truncate)
import           System.FilePath.Posix

-- | Returns information about file at 'FilePath'
filestat :: FS m => FilePath -> m FileStat
filestat path = do
  FSS { .. } <- get
  return case getINodeByPath path inodes mem of
    Right i -> fileStat i
    Left  e -> error $ show e

-- | Creates new file at prefix 'FilePath' with name postfix 'FilePath'
create :: FS m => FilePath -> m ()
create path = do
  st@FSS { inodes, inodeBitMap, blockBitMap, mem } <- get

  -- TODO: how to check it smoothly
  let _ = case getINodeByPath path inodes mem of
            Left e -> e
            _      -> error $ show EEXIST
  let index = case bitIndex 0 $ unIbm inodeBitMap of
                Just i  -> i
                Nothing -> error $ show ENOSPC

  -- allocating mem and writing a filename to the first block
  let (newBlocks, newBMap) = findNFreeBlocks blockBitMap 1
      newIndex = fromIntegral $ head newBlocks
      newName = putName (toVectorWord8 $ BS.pack path)
              $ unBlock $ mem !! newIndex

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
  let dirIndex = case getINodeIndexByPath dirPath inodes mem of
                   Just i  -> i
                   Nothing -> error $ show ENXIST

  let INode { blocks } = inodes !! dirIndex

  dir <- readDir blocks
  newDir <- writeDir (dir <> BS.pack ("\n" <> path <> ";" <> show index)) blocks

  put $ st { mem = newDir }


-- | Opens file at 'FilePath' returning 'FileDescriptor' for future uses
open :: FS m => FilePath -> m FileDescriptor
open path = do
  st@FSS { inodes, fdlist, mem } <- get
  let x = case getINodeByPath path inodes mem of
            Right INode { blocks } -> FileDescriptor (length fdlist) blocks
            Left  e                -> error $ show e

  put $ st { fdlist=fdlist <> [x] }

  return x

-- | Closes given 'FileDescriptor'
close :: FS m => FileDescriptor -> m ()
close FileDescriptor { unID } = do
  st@FSS { fdlist } <- get
  put $ st { fdlist = filter (\(FileDescriptor fid _ ) -> unID /= fid) fdlist }

-- | Reads 'Int' bytes at 'Int' offset from 'FileDescriptor'
read :: FS m => FilePath -> FileDescriptor -> Int -> Int -> m (V.Vector Word8)
read _ FileDescriptor { unBlocks } bytes off =
  V.take bytes . V.drop off <$> fromIndToVec unBlocks


-- | Writes 'Int' bytes at 'Int' offset to 'Filedescriptor'
write :: FS m => FilePath -> FileDescriptor -> Int -> Int -> V.Vector Word8 -> m Int
write _ FileDescriptor { unBlocks } bytes off d = do
  st@FSS { metadata=SBlock { blockSize }, mem} <- get

  fdBlocks <- fromIndToVec unBlocks
  when (off + bytes < V.length fdBlocks) (error $ show ENOMEM)

  let newMem = writeMem (fromIntegral blockSize) mem unBlocks (V.take bytes d) (V.length d)

  put $ st { mem = newMem }

  return bytes

-- | Creates hard link in 'FilePath' to 'FilePath'
link :: FS m => FilePath -> FilePath -> m ()
link src dst = do
  st@FSS { inodes, mem } <- get
  let index = case getINodeIndexByPath src inodes mem of
                 Just i -> i
                 Nothing -> error $ show ENXIST

  let ino@INode { fileStat=FS{ fileType }
                , linkCount
                } = inodes !! index

  -- checking if src is not a dir
  let _ = case fileType of
            None      -> error $ show ENXIST
            Directory -> error $ show EISDIR
            _         -> False

  let dirPath = dropFileName src
      dirIndex = case getINodeIndexByPath dirPath inodes mem of
                   Just i  -> i
                   Nothing -> error $ show ENXIST

  let INode { blocks } = inodes !! dirIndex
      indexDst = case getINodeIndexByPath dst inodes mem of
                   Just i  -> i
                   Nothing -> error $ show ENXIST

  -- updating dir cont*nt
  dir <- readDir blocks
  newDir <- writeDir (dir <> BS.pack ("\n" <> dst <> ";" <> show indexDst)) blocks

  let newIno = ino { linkCount = linkCount+1 }

  put $ st { inodes = init $ take index inodes
                     <> pure newIno
                     <> drop index inodes
           , mem = newDir
           }

-- | Destroys link at 'FilePath'
unlink :: FS m => FilePath -> m ()
unlink path = do
  st@FSS { .. } <- get

  let index = case getINodeIndexByPath path inodes mem of
            Just i  -> i
            Nothing -> error $ show ENXIST

  let INode { linkCount } = inodes !! index
  -- TODO: unlink should exit if this `if` statement is correct
  when (linkCount > 1) $ rmEntryFromDir path $ dropFileName path

  (ni, nim, nb) <- unlinkINodeByPath path inodes inodeBitMap blockBitMap
  put st { inodes = ni
         , inodeBitMap = nim
         , blockBitMap = nb
         }

-- | Truncates file at 'FilePath' to 'Int' bytes
truncate :: FS m => FilePath -> Int -> m ()
truncate path size = do
  st@FSS { metadata=SBlock{ blockSize }, inodes, mem, blockBitMap} <- get

  let ino@INode { blockCount } = case getINodeByPath path inodes mem of
               Right i -> i
               Left  e -> error $ show e

  let index = case getINodeIndexByPath path inodes mem of
            Just i  -> i
            Nothing -> error $ show ENXIST

  let (newBlocks, newBitMap) = findNFreeBlocks
                               blockBitMap
                               (size - fromIntegral (blockCount * blockSize))

  -- if size > size of blocks of inodes
  let newInos = init $ take index inodes
             <> pure (ino { blocks=newBlocks })
             <> drop index inodes

  put st { inodes=newInos, blockBitMap=newBitMap }


-- | Creates directory at 'FilePath'
mkdir :: FS m => FilePath -> m ()
mkdir path = do
  st@FSS { inodes, inodeBitMap, blockBitMap, mem } <- get

  -- TODO: how to check it smoothly
  let _ = case getINodeByPath path inodes mem of
            Left e -> e
            _      -> error $ show EEXIST
  let index = case bitIndex 0 $ unIbm inodeBitMap of
            Just i  -> i
            Nothing -> error $ show ENOSPC


  let (newBlocks, newBMap) = findNFreeBlocks blockBitMap 1
      newIndex = fromIntegral $ head newBlocks
      newName = putName (toVectorWord8 $ BS.pack path)
             $ unBlock $ mem !! newIndex

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

  let INode { blocks } = case getINodeByPath path inodes mem of
            Right i -> i
            _       -> error $ show ENXIST

  dirBlob <- readDir blocks
  dirLinks <- readDirLinks dirBlob
  mapM_ unlink dirLinks

  unlink $ dropFileName path

-- TODO rewrite to create style
-- | Creates symbolic link from 'FilePath' to 'FilePath'
symlink :: FS m => FilePath -> FilePath -> m ()
symlink src dst = do
  -- try to create file and catch error in this case
  let v = toVectorWord8 $ BS.pack dst
  -- changing size of src file to the size of ther dst link
  truncate src $ V.length v

  fd <- open src
  _ <- write src fd (V.length v) 0 v
  close fd

-- | Filters list of inodes and returns the first one which name
-- matches path
getINodeByPath :: FilePath -> [INode] -> [Block] -> Either FSError INode
getINodeByPath "" _ _ = Left ENXIST
getINodeByPath path inodes mem =
  case filter f inodes of
    (x:_) -> Right x
    _     -> Left ENXIST
  where f = \i -> path == getName
                  (map (\el -> mem !! fromIntegral el) (blocks i))

-- | Finds inode index by filename
getINodeIndexByPath :: FilePath -> [INode] -> [Block] -> Maybe Int
getINodeIndexByPath = go 0
  where
    go :: Int -> FilePath -> [INode] -> [Block] -> Maybe Int
    go acc p (i:is) mem =
          if p == getName (map (\el -> mem !! fromIntegral el) (blocks i))
            then Just acc
            else go (acc + 1) p is mem
    go _ _ _ _ = Nothing

-- | Unlinks entity from INode
unlinkINodeByPath :: FS m => FilePath
                          -> [INode]
                          -> INodeBitMap
                          -> BlockBitMap
                          -> m ([INode], INodeBitMap, BlockBitMap)
unlinkINodeByPath path inodes imap bmap = do
  FSS { mem } <- get
  let inmap = unIbm imap
      ibmap = unBbm bmap
  -- index of inode to unlink
  let index = case getINodeIndexByPath path inodes mem of
            Just i  -> i
            Nothing -> error $ show ENXIST

  let INode { blocks } = inodes !! index

  let newBMap = ibmap V.// go blocks []
          where go :: [Index Block] -> [(Int, Bit)] -> [(Int, Bit)]
                go (i:is) vec = go is $ vec <> pure (fromIntegral i, Bit False)
                go _ vec      = vec
      newInos = init (take index inodes)
             <> pure (INode 0 0 (FS 0 None) [])
             <> drop index inodes
      newIMap = V.init (V.take index inmap)
             <> V.singleton (Bit False)
             <> V.drop index inmap

  return (newInos, Ibm newIMap, Bbm newBMap)

-- | Removes given filepath from directory
rmEntryFromDir :: FS m => FilePath -> FilePath -> m ()
rmEntryFromDir fpath dpath = do
  st@FSS { inodes, mem } <- get
  let INode { blocks } = case getINodeByPath dpath inodes mem of
                           Right i -> i
                           Left e  -> error $ show e
  dir <- readDir blocks
  let newDir = foldr (<>) BS.empty
               $ filter (\bs -> BS.unpack
                 (head $ BS.splitWith (== ';') bs) /= fpath)
               $ BS.splitWith (== '\n') dir
  newMem <- writeDir newDir blocks

  put $ st { mem = newMem }

-- | Converts indices to Vector
fromIndToVec :: FS m => [Index Block] -> m (V.Vector Word8)
fromIndToVec (_:inds) = do
  FSS { mem } <- get
  return $ go mem inds V.empty

  where go :: [Block] -> [Index Block] -> V.Vector Word8 -> V.Vector Word8
        go mem (i:is) acc = go mem is $ acc V.++ unBlock (mem !! fromIntegral i)
        go _   []     acc = acc
fromIndToVec _ = error $ show EFAULT

-- | Find N free blocks
findNFreeBlocks :: BlockBitMap -> Int -> ([Index Block], BlockBitMap)
findNFreeBlocks Bbm {..} n = let blockMap = unBbm in
  go blockMap n (V.length blockMap) []
  where go :: V.Vector Bit -> Int -> Int -> [Index Block] -> ([Index Block], BlockBitMap)
        go _ _ 0 _ = error $ show ENOSPC
        go bmap count off acc =
          case bmap V.! (V.length bmap - off) of
            Bit False -> if count == 0
                           then (acc <> pure (fromIntegral $ V.length bmap - off), Bbm bmap)
                           else go (modMap bmap off) (count - 1) (off - 1) acc
            Bit True  -> go bmap count (off - 1) acc

        modMap bmap off = V.modify (`flipBit` (V.length bmap - off)) bmap

-- | Writes Vector Word8 to blocks under given indices
writeMem :: Int -> [Block] -> [Index Block] -> V.Vector Word8 -> Int -> [Block]
writeMem _ mem _ _ 0 = mem
writeMem bsize mem (_:b:bs) v vecOff = do
  writeMem bsize
         (init (take (fromIntegral b) mem)
          <> pure (Block
                -- if number of bytes to write is less than
                -- block size than write them only to
                -- a part of vector
                if bsize <= vecOff
                  then (V.++)
                       (unBlock (last $ take (fromIntegral b) mem))
                       (V.take bsize (V.drop (V.length v - vecOff) v))
                  else V.take bsize $ V.drop (V.length v - vecOff) v)
          <> drop (fromIntegral b) mem)
         bs
         v
         (if bsize <= vecOff
            then 0
            else vecOff - bsize)
writeMem _ mem _ _ _ = mem

-- | Reads list of inodes indices from blocks
readDir :: FS m => [Index Block] -> m BS.ByteString
readDir (_:index) = toByteString <$> fromIndToVec index
readDir _ = error $ show EFAULT

-- | Retract filepathes from dir cont*nt
readDirLinks :: FS m => BS.ByteString -> m [FilePath]
readDirLinks b = return . map (BS.unpack . head . BS.splitWith (== ';'))
               $ BS.splitWith (== '\n') b


-- | Writes bytestring to the given blocks
writeDir :: FS m => B.ByteString -> [Index Block] -> m [Block]
writeDir vData (_:ind) = do
  FSS { metadata=SBlock { blockSize }, mem } <- get

  let nData = toVectorWord8 vData
      bSize = fromIntegral blockSize

  return $ writeMem bSize mem ind nData $ V.length nData
writeDir _ _ = error $ show EFAULT

-- | Writes copies first vector in the beginning of the 2nd vector,
-- which ought to be larger than the 1st one
putName :: V.Vector Word8  -> V.Vector Word8  -> V.Vector Word8
putName src dst = V.slice 0 (V.length dst) src

-- | Reads Blocks and returns the name of the file
getName :: [Block] -> FilePath
getName (x:_) = BS.unpack . toByteString . unBlock $ x
getName _ = error $ show EFAULT
