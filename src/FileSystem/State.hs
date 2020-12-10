{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : FileSystem.State
-- Description : Filesystem internal representation of state
--
-- This module contains types and functions which are used for internal representation of the filesystem
module FileSystem.State
  ( FState (..),
    FS,
    bytesToWord64,
  )
where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bits
import qualified Data.ByteString      as B
import           Data.Word
import           FileSystem.Internal

data FState = FSS
  { metadata    :: SuperBlock
  , blockBitMap :: BlockBitMap
  , inodeBitMap :: INodeBitMap
  , inodes      :: [INode]
  , mem         :: [Block]
  , fdlist      :: [FileDescriptor]
  }

type FS m = (MonadState FState m, MonadError FSError m)

-- data SuperBlock = SBlock
-- { blockSize :: Word64,
-- blockCount :: Word64,
-- freeBlocks :: Word64,
-- freeINodes :: Word64
-- }
-- deriving (Show, Eq)

-- | Converts list of 8 bytes to Word64, failing if list isn't long enough
bytesToWord64 :: [Word8] -> Maybe Word64
bytesToWord64 bs = if length bs == 8
                   then go bs 0 0
                   else Nothing
  where
    go :: [Word8] -> Word8 -> Word64 -> Maybe Word64
    go _ 8 r         = Just r
    go (x : xs) ct r = go xs (ct + 1) . flip shift 7 $ fromIntegral x .|. r
    go _ _ _         = Nothing

-- | Demarshall SuperBlock from ByteString
getSuperBlock :: B.ByteString -> Maybe SuperBlock
getSuperBlock b = case B.uncons b of
  Just (h, t) -> Nothing
  Nothing     -> Nothing

getBlockBitMap :: B.ByteString -> BlockBitMap
getBlockBitMap = undefined

getINodeBitMap :: B.ByteString -> INodeBitMap
getINodeBitMap = undefined

getINode :: B.ByteString -> INode
getINode = undefined

getBlock :: B.ByteString -> Block
getBlock = undefined

loadFS :: B.ByteString -> FState
loadFS = undefined
