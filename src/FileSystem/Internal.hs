-- |
-- Module      : FileSystem.Internal
-- Description : Filesystem internals
--
-- This module contains types and functions which are used for internal representation of the filesystem
module FileSystem.Internal
  ( BlockBitMap
  , INodeBitMap
  , SuperBlock (..)
  , INode (..)
  , Index
  , FileType (..)
  , Block (..)
  , FileStat (..)
  , FSError (..)
  , FileDescriptor (..)
  , readDir
  , readLink
  , getName
  , putName
  , toByteString
  , toVectorWord8
  )
where

import           Data.Bit
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as V
import           Data.Word
import           Prelude

-- | Bitmap for blocks
-- 1 indicating that block is used
-- 0 indicating that block is free
type BlockBitMap = V.Vector Bit

-- | Bitmap for INodes
-- 1 indicating that inode is used
-- 0 indicating that inode is free
type INodeBitMap = V.Vector Bit

-- | SuperBlock is a first block in whole file system
-- which contains metadata information
data SuperBlock = SBlock
  { blockSize  :: Word64
  , blockCount :: Word64
  , freeBlocks :: Word64
  , freeINodes :: Word64
  } deriving (Show, Eq)

-- | Type of file that INode "holds"
-- 'File' - regular file on disk
-- 'Directory' - directory
-- 'Link' - symbolic link
data FileType = File | Directory | Link deriving (Show, Eq, Enum)

data FileStat = FS
  { size     :: Int
  , fileType :: FileType
  } deriving (Show, Eq)


----------------------------INDEX---------------------------------

-- | Index to either Block or INode
type Index a = Word64


-----------------------INODE AND BLOCK----------------------------

-- | INode is a structure that holds metadata about file
data INode = INode
  { blockCount :: Word64
  , fileStat   :: FileStat
  , blocks     :: [Index Block]
  } deriving (Show, Eq)

-- | Block in memory, which is actually a vector of bytes with fixed size
newtype Block = Block { unBlock :: V.Vector Word8 } deriving (Show, Eq)

-- | File descriptor, which contains list of block indices
data FileDescriptor = FileDescriptor
  { unID     :: Int
  , unBlocks :: [Index Block]
  } deriving (Show, Eq)


---------------------------ERRORS---------------------------------

data FSError = EEXIST | ENXIST | ENOSPC | ENOMEM deriving (Eq, Show)

--------------------------FUNCTIONS-------------------------------

-- | Reads list of inodes indices from blocks
readDir :: [Index Block] -> [Index INode]
readDir = undefined

-- | Reads hardlink
readLink :: [Index Block] -> Index INode
readLink = undefined

-- | Reads Blocks and returns the name of the file
getName :: [Index Block] -> FilePath
getName = undefined

-- | Writes copies first vector in the beginning of the 2nd vector,
-- which ought to be larger than the 1st one
putName :: V.Vector a -> V.Vector a -> V.Vector a
putName = undefined

-- | Converts Vector Word8 to ByteString
toByteString :: V.Vector Word8 -> B.ByteString
toByteString = V.foldr (flip B.snoc) B.empty

-- | Converts ByteString to Vector Word8
toVectorWord8 :: B.ByteString -> V.Vector Word8
toVectorWord8 b = foldr ((V.++) . V.singleton) V.empty (B.unpack b)