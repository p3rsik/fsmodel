{-# LANGUAGE FlexibleContexts #-}

module FileSystem
  ( filestat,
    create,
    open,
    close,
    read,
    write,
    link,
    unlink,
    truncate,
    mkdir,
    rmdir,
    symlink,
  )
where

import Data.Vector hiding (create)
import Data.Word
import FileSystem.Internal
import FileSystem.State
import Prelude hiding (read, truncate)

-- | Returns information about file at 'FilePath'
filestat :: FS m => FilePath -> m FileStat
filestat = undefined

-- | Creates new file at prefix 'FilePath' with name postfix 'FilePath'
create :: FS m => FilePath -> m ()
create = undefined

-- | Opens file at 'FilePath' returning 'FileDescriptor' for future uses
open :: FS m => FilePath -> m FileDescriptor
open = undefined

-- | Closes given 'FileDescriptor'
close :: FS m => FileDescriptor -> m ()
close = undefined

-- | Reads 'Int' bytes at 'Int' offset from 'FileDescriptor'
read :: FS m => FileDescriptor -> Int -> Int -> m (Vector Word8)
read = undefined

-- | Writes 'Int' bytes at 'Int' offset to 'Filedescriptor'
write :: FS m => FileDescriptor -> Int -> Int -> Vector Word8 -> m Int
write = undefined

-- | Creates symbolic link from 'FilePath' to 'FilePath'
link :: FS m => FilePath -> FilePath -> m ()
link = undefined

-- | Destroys symbolic link at 'FilePath'
unlink :: FS m => FilePath -> m ()
unlink = undefined

-- | Truncates file at 'FilePath' to 'Int' bytes
truncate :: FS m => FilePath -> Int -> m ()
truncate = undefined

-- | Creates directory at 'FilePath'
mkdir :: FS m => FilePath -> m ()
mkdir = undefined

-- | Removes directory at 'FilePath'
rmdir :: FS m => FilePath -> m ()
rmdir = undefined

-- | Creates soft symbolic link from 'FilePath' to 'FilePath'
symlink :: FS m => FilePath -> FilePath -> m ()
symlink = undefined
