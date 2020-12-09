module FileSystem
  ( ls,
    cd,
    filestat,
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

import Prelude hiding (read, truncate)
import Data.Vector hiding (create)
import Data.Word
import FileSystem.Internal

filestat :: INode -> FileStat
filestat = undefined

ls :: FilePath -> [File]
ls = undefined

create :: FilePath -> ()
create = undefined

open :: FilePath -> FileDescriptor
open = undefined

close :: FileDescriptor -> ()
close = undefined

read :: FileDescriptor -> Int -> Int -> Vector Word8
read = undefined

write :: FileDescriptor -> Int -> Int -> Vector Word8 -> Either FSError Int
write = undefined

link :: FilePath -> FilePath -> ()
link = undefined

unlink :: FilePath -> ()
unlink = undefined

truncate :: FilePath -> Int -> ()
truncate = undefined

mkdir :: FilePath -> ()
mkdir = undefined

rmdir :: FilePath -> ()
rmdir = undefined

cd :: FilePath -> ()
cd = undefined

symlink :: FilePath -> FilePath -> ()
symlink = undefined
