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
  FSS { inodes } <- get
  return case getINodeByPath path inodes of
    Right i -> fileStat i
    Left  e -> error $ show e

-- | Creates new file at prefix 'FilePath' with name postfix 'FilePath'
create :: FS m => FilePath -> m ()
create path = do
  st@FSS { inodes, inodeBitMap, blockBitMap, mem } <- get

  -- TODO: how to check it smoothly
  let _ = case getINodeByPath path inodes of
            Left e -> e
            _      -> error $ show EEXIST
  let index = case bitIndex 0 $ unIbm inodeBitMap of
            Just i  -> i
            Nothing -> error $ show ENOSPC

  let ino = inodes !! index

  let (newBlocks, newBMap) = findNFreeBlocks blockBitMap 1
  let newIndex = fromIntegral $ head newBlocks
  let newMem = init $ take newIndex mem
            <> pure (Block $ putName (toVectorWord8 $ BS.pack path)
                           $ unBlock $ mem !! newIndex)
            <> drop newIndex mem


  let ni = ino { blockCount = 1
               , fileStat = FS (fromIntegral $ length path) File
               , blocks = newBlocks
               }

  -- TODO: update directory cont*nt with this file
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

-- | Opens file at 'FilePath' returning 'FileDescriptor' for future uses
open :: FS m => FilePath -> m FileDescriptor
open path = do
  st@FSS { inodes, fdlist } <- get
  let x = case getINodeByPath path inodes of
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
  FSS { metadata=SBlock { blockSize }, mem} <- get

  fdBlocks <- fromIndToVec unBlocks
  when (off + bytes < V.length fdBlocks) (error $ show ENOMEM)

  let _ = go (fromIntegral blockSize) mem unBlocks (V.take bytes d) (V.length d)

  return bytes
  where
    go :: Int
       -> [Block] -- mem
       -> [Index Block] -- blocks read from fd
       -> V.Vector Word8 -- data to write
       -> Int -- offset for data to write
       -> [Block] -- new mem
    go _ mem _ _ 0 = mem
    go bsize mem (b:bs) v vecOff = do
      go bsize
         (init (take (fromIntegral b) mem)
          <> [Block
                -- if number of bytes to write is less than
                -- block size than write them only to
                -- a part of vector
                if bsize <= vecOff
                  then (V.++)
                       (unBlock (last $ take (fromIntegral b) mem))
                       (V.take bsize (V.drop (V.length v - vecOff) v))
                  else V.take bsize $ V.drop (V.length v - vecOff) v]
          <> drop (fromIntegral b) mem)
         bs
         v
         (if bsize <= vecOff
            then 0
            else vecOff - bsize)
    go _ mem _ _ _ = mem

-- | Creates hard link in 'FilePath' to 'FilePath'
link :: FS m => FilePath -> FilePath -> m ()
link src dst = undefined

-- | Destroys link at 'FilePath'
unlink :: FS m => FilePath -> m ()
unlink path = do
  st@FSS { .. } <- get

  (newInos, newIMap, newBMap) <- unlinkINodeByPath path inodes inodeBitMap blockBitMap
  put st { inodes=newInos
         , inodeBitMap=newIMap
         , blockBitMap=newBMap
         }

-- | Truncates file at 'FilePath' to 'Int' bytes
truncate :: FS m => FilePath -> Int -> m ()
truncate path size = do
  st@FSS { metadata=SBlock{ blockSize }, inodes, blockBitMap} <- get

  let ino@INode { blockCount } = case getINodeByPath path inodes of
               Right i -> i
               Left  e -> error $ show e

  let index = case getINodeIndexByPath path inodes of
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
mkdir path = undefined

-- | Removes directory at 'FilePath'
rmdir :: FS m => FilePath -> m ()
rmdir = unlink

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
getINodeByPath :: FilePath -> [INode] -> Either FSError INode
getINodeByPath "" _ = Left ENXIST
getINodeByPath path inodes =
  case filter f inodes of
    (x:_) -> Right x
    _     -> Left ENXIST
  where f = \i -> path == getName (blocks i)

-- | Finds inode index by filename
getINodeIndexByPath :: FilePath -> [INode] -> Maybe Int
getINodeIndexByPath path inodes = go inodes path 0
  where
    go :: [INode] -> FilePath -> Int -> Maybe Int
    go (i:is) p acc =
          if p == getName (blocks i)
            then Just acc
            else go is p $ acc + 1
    go _ _ _ = Nothing

-- | Unlinks entity from INode
unlinkINodeByPath :: FS m => FilePath
                          -> [INode]
                          -> INodeBitMap
                          -> BlockBitMap
                          -> m ([INode], INodeBitMap, BlockBitMap)
unlinkINodeByPath path inodes imap bmap = do
  let inmap = unIbm imap
  let ibmap = unBbm bmap
  -- index of inode to unlink
  let index = case getINodeIndexByPath path inodes of
            Just i  -> i
            Nothing -> error $ show ENXIST

  let INode { blocks } = inodes !! index
  let newBMap = ibmap V.// go blocks []
          where go :: [Index Block] -> [(Int, Bit)] -> [(Int, Bit)]
                go (i:is) vec = go is $ vec <> pure (fromIntegral i, Bit False)
                go _ vec      = vec

  let newInos = init (take index inodes)
             <> pure (INode 0 (FS 0 File) [])
             <> drop index inodes
  let newIMap = V.init (V.take index inmap)
             <> V.singleton (Bit False)
             <> V.drop index inmap

  return (newInos, Ibm newIMap, Bbm newBMap)


-- | Converts indices to Vector
fromIndToVec :: FS m => [Index Block] -> m (V.Vector Word8)
fromIndToVec inds = do
  FSS { mem } <- get
  return $ go mem inds V.empty

  where go :: [Block] -> [Index Block] -> V.Vector Word8 -> V.Vector Word8
        go mem (i:is) acc = go mem is $ acc V.++ unBlock (mem !! fromIntegral i)
        go _   []     acc = acc

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
