module FileSystem.Internal
  ( checkExist
  , decINOSz
  , insert
  , getName
  , getINOByPath
  , findNFreeBlocks
  , fromIndToVec
  , fromVecToBlock
  , putName
  , readDir
  , readDirLinks
  , rmEntryFromDir
  , toByteString
  , toVectorWord8
  , trimNull
  , unlinkINodeByPath
  , writeDir
  , writeMem
  ) where

import           Control.Monad.State
import           Control.Monad.Except
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
getINOByPath path ind bl = go 0 path ind bl
  where
    go :: FS m => Int -> FilePath -> [INode] -> [Block] -> m (Int, INode)
    go _ _ _ [] = throwError EFAULT
    go acc p (i:is) mem =
          if p == (trimNull . getName)
                  (map (\el -> mem !! fromIntegral el) (blocks i))
          then return (acc, ind !! acc)
          else go (acc + 1) p is mem
    go _ _ _ _ = throwError ENXIST

-- | Checks if file exists
checkExist :: FS m => FilePath -> [INode] -> [Block] -> m Bool
checkExist path ind bl = catchError
  (getINOByPath path ind bl >> return True)
  (const $ return False)

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
fromIndToVec _ = return V.empty

-- | Find N free blocks
findNFreeBlocks :: BlockBitMap -> Int -> ([Index Block], BlockBitMap)
findNFreeBlocks Bbm {..} n = go unBbm n (V.length unBbm) []
  where go :: V.Vector Bit -> Int -> Int -> [Index Block] -> ([Index Block], BlockBitMap)
        go _ _ 0 _ = error $ show ENOSPC
        go bmap count off acc =
          case bmap V.! (V.length bmap - off) of
            Bit False -> if count == 1
                           then (acc <> pure (index off), Bbm $ modMap bmap off)
                           else go (modMap bmap off)
                                   (count - 1)
                                   (off - 1)
                                   (acc <> pure (index off))
            Bit True  -> go bmap count (off - 1) acc

        modMap bmap off = V.modify (`flipBit` (V.length bmap - off)) bmap
        index off = fromIntegral $ V.length unBbm - off :: Index Block

-- | Writes Vector Word8 to blocks under given indices
writeMem :: Int -- ^ Block size
         -> [Block]
         -> [Index Block]
         -> V.Vector Word8 -- ^ data to write
         -> Int -- ^ offset
         -> [Block]
writeMem _ mem _ _ 0 = mem
writeMem bsize mem blk vec off = offBl : go bsize mem (drop (fst offset) blk) vec off
  where go :: Int -> [Block] -> [Index Block] -> V.Vector Word8 -> Int -> [Block]
        go bsz m (b:bs) vec off = fromVecToBlock vec off
        oc = off `div` bsize
        om = off `mod` bsize
        offset = (oc, om) -- offset in list and in block
        -- fix offset
        offBl = Block . V.take (snd offset) . unBlock
               $ mem !! fromIntegral (head $ take (fst offset) blk)
writeMem _ mem _ _ _ = mem

-- | ...data, Block size
fromVecToBlock :: V.Vector Word8 -> Int -> [Block]
fromVecToBlock vec bsize = go vec 0
  where go :: V.Vector Word8 -> Int -> [Block]
        go v fo = if V.length v <= fo
                  then []
                  else Block (dl v fo V.++ rp v fo) : go v (fo + bsize)
        dl v fo = V.take bsize $ V.drop fo v :: V.Vector Word8
        rp v fo = V.replicate (bsize - V.length (dl v fo)) 0 :: V.Vector Word8

-- | Reads list of inodes indices from blocks
readDir :: FS m => [Index Block] -> m BS.ByteString
readDir index = toByteString <$> fromIndToVec index
readDir _ = return BS.empty

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
writeDir _ _ = throwError EFAULT

-- | Creates Vector Word8 with the given size
-- and bytestring at the beginning
putName :: B.ByteString -> Int -> V.Vector Word8
putName src size
  | size < B.length src = V.fromListN size $ B.unpack src
  | otherwise = (V.++)
                (V.fromList $ B.unpack src)
                $ V.generate len f
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

-- | Decreases size of inode's mem
decINOSz :: [Index Block] -> Int -> BlockBitMap -> ([Index Block], BlockBitMap)
decINOSz blk 0  bmap = (blk, bmap)
decINOSz blk sz bmap = (take sz blk, go (drop (sz - 1) blk) bmap)
  where go :: [Index Block] -> BlockBitMap -> BlockBitMap
        go [] bm = bm
        go (i:is) bm = go is $ fp bm $ fromIntegral i
        fp bm index = Bbm . V.modify (`flipBit` index) $ unBbm bm

-- | Insert element into list
insert :: a -> Int -> [a] -> [a]
insert el 0   lst = [el] <> drop 1 lst
insert el pos lst = init $ take pos lst
                 <> [el]
                 <> drop pos lst

-- | Converts Vector Word8 to ByteString
toByteString :: V.Vector Word8 -> B.ByteString
toByteString = B.pack . V.toList

-- | Converts ByteString to Vector Word8
toVectorWord8 :: B.ByteString -> V.Vector Word8
toVectorWord8 = V.fromList . B.unpack