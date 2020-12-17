{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : FileSystem.State
-- Description : Filesystem internal representation of state
--
-- This module contains types and functions which are used for internal representation of the filesystem
module FileSystem.State
  ( FState (..)
  , FS
  , createFS
  )
where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bit
import qualified Data.Vector.Unboxed  as V
import Data.ByteString (unpack)
import           Data.Word
import           FileSystem.Internal

data FState = FSS
  { metadata    :: !SuperBlock
  , blockBitMap :: !BlockBitMap
  , inodeBitMap :: !INodeBitMap
  , inodes      :: ![INode]
  , mem         :: ![Block]
  , fdlist      :: ![FileDescriptor]
  } deriving (Show, Eq)

type FS m = (MonadState FState m, MonadError FSError m)

-- | Create new FileSystem with 'Word64' block size,
-- 'Word64' block amount and 'Word64' inode amount
createFS :: Word64 -> Word64 -> Word64  -> FState
createFS bSize bCount iCount  = FSS {..}
  where
    metadata = SBlock bSize bCount bCount iCount iCount
    -- First Block and INode is reserved for root (/)
    blockBitMap = Bbm . V.cons (Bit True) . V.replicate (fromIntegral bCount - 1) $ Bit False
    inodeBitMap = Ibm . V.cons (Bit True) . V.replicate (fromIntegral iCount - 1) $ Bit False
    inodes = INode 1 0 (FS 1 Directory) [1] :
      replicate (fromIntegral iCount - 1) (INode 0 0 (FS 0 None) [])
    mem = Block (V.fromList (unpack "/") <> V.replicate (fromIntegral  bSize - 1) 0) :
      replicate (fromIntegral bCount - 1) (Block $ V.replicate (fromIntegral bSize) 0)
    fdlist :: [FileDescriptor]
    fdlist = []
