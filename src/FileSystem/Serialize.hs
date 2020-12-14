{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module FileSystem.Serialize
  (Serialize (..)
  , encodeBlock
  , decodeBlock
  )
where

import           Data.Bit
import           Data.Bits
import qualified Data.ByteString      as B
-- import           Data.Vector.Storable
import qualified Data.Vector.Unboxed  as V
import           Data.Word
import           FileSystem.Internal
import FileSystem.State

class Serialize a where
  type SizeOf a :: *
  -- | Size of 'a' in bytes
  sizeOf :: SizeOf a
  -- | Encodes 'a' in 'ByteString'
  encode :: a -> B.ByteString
  -- | Decodes 'a' from 'ByteString', can possibly fail
  decode :: B.ByteString -> Maybe a

instance Serialize Word64 where
  type SizeOf Word64 = Int
  sizeOf = 8
  encode = undefined
  decode b
    | B.length b >= sizeOf @Word64 = Just . flip shiftR 8 $ B.foldl' bytesToWord64 0 b
    | otherwise = Nothing
    where
      bytesToWord64 :: Word64 -> Word8 -> Word64
      bytesToWord64 acc el = flip shift 8 $ fromIntegral el .|. acc

-- TODO: Decide what to do with this functions, do we need them at all?
-- toWord8 :: Word64 -> [Word8]
-- toWord8 r = [fromIntegral . shiftR r $ 8 * i | i <- [3, 2, 1, 0]]

-- fromWord8 :: [Word8] -> Word64
-- fromWord8 = foldl

instance Serialize SuperBlock where
  type SizeOf SuperBlock = Int
  sizeOf = 40
  encode = undefined
  decode b
    | B.length b >= 40 = SBlock <$> bs <*> bc <*> fb  <*> ic <*> fi
    | otherwise = Nothing
    where
      bs = decode $ B.take 8 b
      bc = decode . B.take 8 $ B.drop 8 b
      fb = decode . B.take 8 $ B.drop 16 b
      ic = decode . B.take 8 $ B.drop 24 b
      fi = decode . B.take 8 $ B.drop 32 b

instance Serialize BlockBitMap where
  type SizeOf BlockBitMap = SuperBlock -> Int
  -- First 8 bytes for size + (amount of blocks * size of block) / 8
  -- because 8 bits(info for 8 blocks) gets packed into 1 byte
  sizeOf SBlock {..} = 8 + fromIntegral (blockCount * blockSize) `div` 8
  encode = undefined
  decode b
    | B.length b >= size =
      Just . Bbm . cloneFromByteString . B.take size $ B.drop 8 b
    | otherwise = Nothing
    where
      size = maybe maxBound fromIntegral $ decode @Word64 b

instance Serialize INodeBitMap where
  type SizeOf INodeBitMap = SuperBlock -> Int
  -- Look at BlockBitMap instance for description
  sizeOf SBlock {..} = 8 + fromIntegral inodeCount `div` 8
  encode = undefined
  decode b
    | B.length b >= size =
      Just . Ibm . cloneFromByteString . B.take size $ B.drop 8 b
    | otherwise = Nothing
    where
      size = maybe maxBound fromIntegral $ decode @Word64 b

instance Serialize FileStat where
  type SizeOf FileStat = Int
  sizeOf = 9
  encode = undefined
  decode b
    | B.length b >= sizeOf @FileStat = flip FS ft <$> size
    | otherwise = Nothing
    where
      size = decode $ B.take 8 b
      ft = toEnum . fromIntegral . B.head $ B.drop 8 b

instance Serialize INode where
  type SizeOf INode = Int
  sizeOf = 8 + sizeOf @FileStat + 8 * inodeMaxBlocks
  encode = undefined
  decode b
    | B.length b >= sizeOf @INode = INode <$> bc <*> fs <*> bs
    | otherwise = Nothing
    where
      bc = decode $ B.take 8 b
      fs = decode @FileStat $ B.drop 8 b
      bs = traverse decode $ [B.drop ((8 + sizeOf @FileStat) + i * 8) b | i <- [0 .. (inodeMaxBlocks - 1)]]

instance Serialize Word8 where
  type SizeOf Word8 = Int
  sizeOf = 1
  encode = undefined
  decode b
    | B.length b >= sizeOf @Word8 = Just $ B.head b
    | otherwise = Nothing

encodeBlock :: Block -> B.ByteString
encodeBlock = B.pack . V.toList . unBlock

decodeBlock :: Int -> B.ByteString -> Maybe Block
decodeBlock size b
  | B.length b >= size = Just . Block . V.fromList . B.unpack $ B.take size b
  | otherwise = Nothing

instance Serialize FState where
  type SizeOf FState = SuperBlock -> Int
  sizeOf = undefined
  encode = undefined
  decode b = do
    sb@SBlock {..} <- decode b
    let bSize = fromIntegral blockSize
        bCount = fromIntegral blockCount
        iCount = fromIntegral inodeCount
        bmSize = sizeOf @BlockBitMap sb
    bbm <- decode . flip B.drop b $ sizeOf @SuperBlock
    let toDrop = sizeOf @SuperBlock + sizeOf @BlockBitMap sb
    ibm <- decode $ B.drop toDrop b
    let toDrop = toDrop + sizeOf @INodeBitMap sb
    ins <- traverse (\i -> decode $ B.drop (toDrop + sizeOf @INode * i) b) [0..iCount - 1]
    let toDrop = toDrop + sizeOf @INode * iCount
    bs <- getBlocks bCount bSize $ B.drop toDrop b
    return $ FSS sb bbm ibm ins bs []
    where
      -- | traverse for ByteString
      go :: (B.ByteString -> Maybe a) -> Int -> Int -> B.ByteString -> Maybe [a]
      go _ _ 0 _ = Just []
      go f s c b = (:) <$> f b <*> go f s (c - 1) (B.drop s b)

      getBlocks :: Int -> Int -> B.ByteString -> Maybe [Block]
      getBlocks c s = go (decodeBlock s) s c

-- data FState = FSS
  -- { metadata    :: SuperBlock
  -- , blockBitMap :: BlockBitMap
  -- , inodeBitMap :: INodeBitMap
  -- , inodes      :: [INode]
  -- , mem         :: [Block]
  -- , fdlist      :: [FileDescriptor]
  -- } deriving (Show, Eq)

