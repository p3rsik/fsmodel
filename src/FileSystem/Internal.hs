-- |
-- Module      : FileSystem.Internal
-- Description : Filesystem internals
--
-- This module contains types and functions which are used for internal representation of the filesystem
module FileSystem.Internal
  ( BlockBitMap,
    INodeBitMap,
    SuperBlock (..),
    INode (..),
    FileType (..),
    Block (..),
    FileStat (..),
    FSError (..),
    FileDescriptor (..),
    File,
  )
where

import           Data.Bit
import           Data.Vector
import           Data.Word

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
  { blockSize  :: Word64
  , blockCount :: Word64
  , freeBlocks :: Word64
  , freeINodes :: Word64
  }
  deriving (Show, Eq)

-- | Type of file that INode "holds"
-- 'File' - actual file on disk
-- 'Directory' - directory
-- 'Link' - symbolic link
data FileType = File | Directory | Link deriving (Show, Eq, Enum)

-- | INode is a structure that holds metadata about file
data INode = INode
  { blockCount :: Word64
  , fileStat   :: FileStat
  , blocks     :: [Word64]
  }
  deriving (Show, Eq)

-- | Block in memory, which is actually a vector of bytes with fixed size
newtype Block = Block {unBlock :: Vector Word8} deriving (Show, Eq)

data FileStat = FS
  { size     :: Int
  , fileType :: FileType
  }
  deriving (Show, Eq)

data FSError

data FileDescriptor

data File
