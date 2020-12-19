module FileSystem.Internal
  ( getName
  , getINOByPath
  , findNFreeBlocks
  , fromIndToVec
  , putName
  , readDir
  , readDirLinks
  , rmEntryFromDir
  , trimNull
  , unlinkINodeByPath
  , writeDir
  , writeMem
  ) where

import           Control.Monad.State
import           Data.Bit
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import qualified Data.Vector.Unboxed   as V
import           Data.Word
import           FileSystem.DataTypes
import           FileSystem.State


-- | Finds inode index by filename
getINOByPath :: FS m => FilePath -> [INode] -> [Block] -> m (Int, INode)
getINOByPath path ind bl = return $ go 0 path ind bl
  where
    go :: Int -> FilePath -> [INode] -> [Block] -> (Int, INode)
    go acc p (i:is) mem =
          -- TODO: check if name is not empty string
          if p == (trimNull . getName)
                  (map (\el -> mem !! fromIntegral el) (blocks i))
          then (acc, ind !! acc)
          else go (acc + 1) p is mem
    go _ _ _ _ = error $ show ENXIST 

-- | Unlinks entity from the INode
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
  index <- fst <$> getINOByPath path inodes mem

  let INode { blocks } = inodes !! index
      newBMap = ibmap V.// go blocks []
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
  INode { blocks } <- snd <$> getINOByPath dpath inodes mem 

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

-- | Creates Vector Word8 with the given size
-- and bytestring at the beginning
putName :: B.ByteString -> Int -> V.Vector Word8
putName src size
  | size < B.length src = V.fromListN size $ B.unpack src
  | otherwise = (V.++)
                (V.generate len f) $
                V.fromList $ B.unpack src
  where len = size - B.length src
        f :: a -> Word8
        f = const . fromIntegral . ord $ '\NUL'

-- | Reads Blocks and returns the name of the file
getName :: [Block] -> FilePath
getName (x:_) = BS.unpack . toByteString . unBlock $ x
getName _ = ""

-- | Removes the right part with \NUL symbols from FilePath
trimNull :: FilePath -> FilePath
trimNull ('\NUL':_) = []
trimNull (x:xs) = x : trimNull xs
trimNull _ = []
