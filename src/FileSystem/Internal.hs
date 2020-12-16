-- |
-- Module      : FileSystem.Internal
-- Description : Filesystem internals
--
-- This module contains types and functions which are used for internal representation of the filesystem
module FileSystem.Internal
  ( BlockBitMap (..)
  , INodeBitMap (..)
  , SuperBlock (..)
  , INode (..)
  , Index
  , FileType (..)
  , Block (..)
  , FileStat (..)
  , FSError (..)
  , FileDescriptor (..)
  , inodeMaxBlocks
  , toByteString
  , toVectorWord8
  )
where

import           Data.Bit
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as V
import           Data.Word

-- | Bitmap for blocks
-- 1 indicating that block is used
-- 0 indicating that block is free
newtype BlockBitMap = Bbm { unBbm :: V.Vector Bit} deriving (Show, Eq)

-- | Bitmap for INodes
-- 1 indicating that inode is used
-- 0 indicating that inode is free
newtype INodeBitMap = Ibm { unIbm :: V.Vector Bit} deriving (Show, Eq)

-- | SuperBlock is a first block in whole file system
-- which contains metadata information
data SuperBlock = SBlock
  { blockSize  :: Word64,
    blockCount :: Word64,
    freeBlocks :: Word64,
    inodeCount :: Word64,
    freeINodes :: Word64
  }
  deriving (Show, Eq)

-- | Type of file that INode "holds"
-- 'None' - nothing, empty INode
-- 'File' - actual file on disk
-- 'Directory' - directory
-- 'Link' - symbolic link
data FileType = None | File | Directory | Link deriving (Show, Eq, Enum)

data FileStat = FS
  { size     :: Word64
  , fileType :: FileType
  } deriving (Show, Eq)


----------------------------INDEX---------------------------------

-- | Index to either Block or INode
type Index a = Word64


-----------------------INODE AND BLOCK----------------------------

-- | INode is a structure that holds metadata about file
data INode = INode
  { blockCount :: Word64
  , linkCount  :: Word64
  , fileStat   :: FileStat
  , blocks     :: [Index Block]
  } deriving (Show, Eq)

inodeMaxBlocks :: Int
inodeMaxBlocks = 16

-- | Block in memory, which is actually a vector of bytes with fixed size
newtype Block = Block { unBlock :: V.Vector Word8 } deriving (Show, Eq)

-- | File descriptor, which contains list of block indices
data FileDescriptor = FileDescriptor
  { unID     :: Int
  , unBlocks :: [Index Block]
  } deriving (Show, Eq)


---------------------------ERRORS---------------------------------

data FSError = EEXIST | EFAULT | EISDIR | ENXIST | ENOSPC | ENOMEM deriving (Eq, Show)

--------------------------FUNCTIONS-------------------------------


-- | Converts Vector Word8 to ByteString
toByteString :: V.Vector Word8 -> B.ByteString
toByteString = V.foldr (flip B.snoc) B.empty

-- | Converts ByteString to Vector Word8
toVectorWord8 :: B.ByteString -> V.Vector Word8
toVectorWord8 b = foldr ((V.++) . V.singleton) V.empty (B.unpack b)
