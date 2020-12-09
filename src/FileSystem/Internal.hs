{-|
Module      : FileSystem.Internal
Description : Filesystem internals

This module contains types and functions which are used for internal representation of the filesystem
-}
module FileSystem.Internal where

import Data.Bit
import Data.Vector
import Data.Word

-- | Bitmap for blocks
-- 1 indicating that block is used
-- 0 indicating that block is free
type BlockBitMap = Vector Bit

-- | Bitmap for INodes
-- 1 indicating that inode is used
-- 0 indicating that inode is free
type INodeBitMap = Vector Bit

-- | SuperBlock is a first block in whole file system
-- which contains metadata information
data SuperBlock = SBlock
  { blockSize :: Int,
    blockCount :: Int,
    freeBlocks :: Int,
    freeINodes :: Int,
    blockBitMap :: BlockBitMap,
    inodeBitMap :: INodeBitMap
  }

-- | Type of file that INode "holds"
-- 'File' - actual file on disk
-- 'Directory' - directory
-- 'Link' - symbolic link
data FileType = File | Directory | Link

-- | INode is a structure that holds metadata about file
data INode = INode
  { blockCount :: Int,
    size :: Int,
    blocks :: [Int],
    fileType :: FileType
  }

-- | Block in memory, which is actually a vector of bytes with fixed size
newtype Block = Block {unBlock :: Vector Word8}

data FileStat = FS
  { size :: Int,
    fileType :: FileType
  }

data FSError

data FileDescriptor

data File
