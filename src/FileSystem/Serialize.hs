{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module FileSystem.Serialize
  (Serialize (..)
  , encodeBlock
  , decodeBlock
  , SError (..)
  , Res
  )
where

import           Control.Monad
import           Data.Bit
import           Data.Bits
import qualified Data.ByteString     as B
import           Data.Foldable
import qualified Data.Vector.Unboxed as V
import           Data.Word
import           FileSystem.DataTypes
import           FileSystem.State

newtype SError = DecodeEr { errMessage :: String} deriving (Eq, Show)

type Res a = Either SError a

class Serialize a where
  type SizeOf a :: *
  -- | Size of 'a' in bytes
  sizeOf :: SizeOf a
  -- | Encodes 'a' in 'ByteString'
  encode :: a -> B.ByteString
  -- | Decodes 'a' from 'ByteString', can possibly fail
  decode :: B.ByteString -> Res a

-- | Helper function to convert between error types
convEr :: (String -> String) -> Either SError a -> Either SError a
convEr f res = case res of
                 Right r -> Right r
                 Left (DecodeEr m) -> Left . DecodeEr $ f m

instance Serialize Word8 where
  type SizeOf Word8 = Int
  sizeOf = 1
  encode = B.singleton
  decode b
    | B.length b >= sizeOf @Word8 = Right $ B.head b
    | otherwise = Left $ DecodeEr "Can't decode Word8: bytestring is too short"

instance Serialize Word64 where
  type SizeOf Word64 = Int
  sizeOf = 8
  encode w = B.pack [fromIntegral . shiftR w $ 8 * i | i <- [7, 6..0]]
  decode b
    | B.length b >= sizeOf @Word64 =
      Right $ b7 .|. fromIntegral (B.head $ B.drop 7 b)
    | otherwise = Left $ DecodeEr "Can't decode Word64: bytestring is too short"
    where
      b7 = B.foldl' bytesToWord64 0 $ B.take 7 b
      bytesToWord64 :: Word64 -> Word8 -> Word64
      bytesToWord64 acc el = flip shift 8 $ fromIntegral el .|. acc

instance Serialize SuperBlock where
  type SizeOf SuperBlock = Int
  sizeOf = 40
  encode SBlock {..} =
    encode blockSize <>
    encode blockCount <>
    encode freeBlocks <>
    encode inodeCount <>
    encode freeINodes
  decode b
    | B.length b >= sizeOf @SuperBlock = SBlock <$> bs <*> bc <*> fb  <*> ic <*> fi
    | otherwise = Left $ DecodeEr "Can't decode SuperBlock: bytestring is too short"
    where
      s = sizeOf @Word64
      conv m = convEr (\x -> "Can't decode " <> m <> ":\n\t" <> x)
      bs = conv "block size" $ decode b
      bc = conv "block count" . decode $ B.drop s b
      fb = conv "free blocks" . decode $ B.drop (s * 2) b
      ic = conv "inode count" . decode $ B.drop (s * 3) b
      fi = conv "free inodes" . decode $ B.drop (s * 4) b

instance Serialize BlockBitMap where
  type SizeOf BlockBitMap = SuperBlock -> Int
  -- First 8 bytes for size + (amount of blocks * size of block) / 8
  -- because 8 bits(info for 8 blocks) gets packed into 1 byte
  sizeOf SBlock {..} = 8 + fromIntegral blockCount `div` 8
  encode Bbm {..} =
    let len = encode @Word64 . fromIntegral $ (V.length unBbm `div` 8) + 8
        bmap = cloneToByteString unBbm
    in len <> bmap
  decode b = case size of
               Right s' -> let si = fromIntegral s' in
                           if B.length b >= si
                           then Right . Bbm . cloneFromByteString . B.take (si - s) $ B.drop s b
                           else Left $ DecodeEr "Can't decode BlockBitMap: the bytestring is too short"
               _ -> Left $ DecodeEr "Can't decode BlockBitMap: Can't get (size :: Word64) from the bytestring"
    where
      size = decode @Word64 b
      s = sizeOf @Word64

instance Serialize INodeBitMap where
  type SizeOf INodeBitMap = SuperBlock -> Int
  -- Look at BlockBitMap instance for description
  sizeOf SBlock {..} = 8 + fromIntegral inodeCount `div` 8
  encode Ibm {..} =
    let len = encode @Word64 . fromIntegral $ (V.length unIbm `div` 8) + 8
        bmap = cloneToByteString unIbm
    in len <> bmap
  decode b = case size of
               Right s' -> let si = fromIntegral s' in
                           if B.length b >= si
                           then Right . Ibm . cloneFromByteString . B.take (si - s) $ B.drop s b
                           else Left $ DecodeEr "Can't decode INodeBitMap: the bytestring is too short"
               _ -> Left $ DecodeEr "Can't decode INodeBitMap: Can't get (size :: Word64) from the bytestring"
    where
      size = decode @Word64 b
      s = sizeOf @Word64

instance Serialize FileType where
  type SizeOf FileType = Int
  sizeOf = sizeOf @Word8
  encode = B.singleton . fromIntegral . fromEnum
  decode b
    | B.length b >= sizeOf @FileType = Right . toEnum . fromIntegral $ B.head b
    | otherwise = Left $ DecodeEr "Can't decode FileType: the bytestring is too short"

instance Serialize FileStat where
  type SizeOf FileStat = Int
  sizeOf = sizeOf @Word64 + sizeOf @FileType
  encode FS {..} =
    let s = encode size
        ft = encode fileType
    in s <> ft
  decode b
    | B.length b >= sizeOf @FileStat = FS <$> size <*> ft
    | otherwise = Left $ DecodeEr "Can't decode FileStat: the bytestring is too short"
    where
      size = decode b
      s = sizeOf @Word64
      ft = decode $ B.drop s b

instance Serialize INode where
  type SizeOf INode = Int
  sizeOf = sizeOf @Word64 * 2 + sizeOf @FileStat + sizeOf @Word64 * inodeMaxBlocks
  encode INode {..} =
    let bc = encode blockCount
        lc = encode linkCount
        fs = encode fileStat
        b = foldl' (<>) B.empty $ encode <$>
          if length blocks <= inodeMaxBlocks
          then blocks <> replicate (inodeMaxBlocks - length blocks) 0
          else  blocks
    in bc <> lc <> fs <> b
  decode b
    | B.length b >= sizeOf @INode = do
        bc <- conv "block count" $ decode b
        lc <- conv "link count" . decode $ B.drop (sizeOf @Word64) b
        fs <- conv "FileStat" . decode $ B.drop (sizeOf @Word64 * 2) b
        let hs = sizeOf @Word64 * 2 + sizeOf @FileStat
        bs <- traverse (conv "Block" . decode) $ [B.drop (hs + i * 8) b
                                            | i <- [0 .. (inodeMaxBlocks - 1)]]
        return . INode bc lc fs $ take (fromIntegral bc) bs
    | otherwise = Left $ DecodeEr "Can't decode INode: the bytestring is too short"
    where
      conv m = convEr (\x -> "Can't decode " <> m <> ":\n\t" <> x)

instance Serialize FState where
  type SizeOf FState = SuperBlock -> Int
  sizeOf sb@SBlock {..} =
    sizeOf @SuperBlock +
    sizeOf @BlockBitMap sb +
    sizeOf @INodeBitMap sb +
    sizeOf @INode * fromIntegral inodeCount +
    fromIntegral (blockSize * blockCount)
  encode FSS {..} =
    let sb = encode metadata
        bmap = encode blockBitMap
        imap = encode inodeBitMap
        ins  = foldl' (<>) B.empty $ encode <$> inodes
        bs   = foldl' (<>) B.empty $ encodeBlock <$> mem
     in sb <> bmap <> imap <> ins <> bs
  decode b = do
    sb@SBlock {..} <- decode b
    let bSize = fromIntegral blockSize
        bCount = fromIntegral blockCount
        iCount = fromIntegral inodeCount
    let toDrop = sizeOf @SuperBlock
    bbm <- decode $ B.drop toDrop b
    let toDrop1 = toDrop + sizeOf @BlockBitMap sb
    ibm <- decode $ B.drop toDrop1 b
    let toDrop2 = toDrop1 + sizeOf @INodeBitMap sb
    ins <- traverse (\i -> decode $ B.drop (toDrop2 + sizeOf @INode * i) b) [0..iCount - 1]
    when (length ins /= iCount) . Left . DecodeEr $ "Decoded " <> show (length ins) <> " inodes, needed " <> show iCount
    let toDrop3 = toDrop2 + sizeOf @INode * iCount
    bs <- getBlocks bCount bSize $ B.drop toDrop3 b
    -- TODO: add import of filepathes into the state
    return $ FSS sb bbm ibm ins [] bs []
    where
      -- | traverse for ByteString
      go :: (B.ByteString -> Res a) -> Int -> Int -> B.ByteString -> Res [a]
      go _ _ 0 _  = Right []
      go f s c bs = (:) <$> f bs <*> go f s (c - 1) (B.drop s bs)

      getBlocks :: Int -> Int -> B.ByteString -> Res [Block]
      getBlocks c s = go (decodeBlock s) s c

encodeBlock :: Block -> B.ByteString
encodeBlock = B.pack . V.toList . unBlock

decodeBlock :: Int -> B.ByteString -> Res Block
decodeBlock size b
  | B.length b >= size = Right . Block . V.fromList . B.unpack $ B.take size b
  | otherwise = Left $ DecodeEr "Can't decode Block: the bytestring is too short"
