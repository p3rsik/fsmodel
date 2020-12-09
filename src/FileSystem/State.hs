{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : FileSystem.State
-- Description : Filesystem internal representation of state
--
-- This module contains types and functions which are used for internal representation of the filesystem
module FileSystem.State
  ( FState (..),
    FS,
  )
where

import Control.Monad.State
import Control.Monad.Except
import FileSystem.Internal

data FState = FSS
  { metadata :: SuperBlock,
    blockBitMap :: BlockBitMap,
    inodeBitMap :: INodeBitMap,
    inodes :: [INode],
    mem :: [Block],
    fdlist :: [FileDescriptor]
  }

type FS m = (MonadState FState m, MonadError FSError m)
