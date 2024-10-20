----------------------------------------------------------------------------
-- |
-- Module      :  Data.Filesystem
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Data.Filesystem
  ( FollowSymlinks(..)
  , findAllCollect
  , findRec
  , AbsDir(..)
  , RelDir(..)
  , AbsFile(..)
  , RelFile(..)
  ) where

import Control.Concurrent.Async
import Control.Monad.Catch
import Data.Coerce
import Data.Foldable
import Data.IORef
import Data.List.NonEmpty (NonEmpty(..))
import Data.NBSem
import GHC.Stack (HasCallStack)
import Prettyprinter.Show
import System.Directory.OsPath.FileType as Streaming
import System.Directory.OsPath.Streaming as Streaming
import System.OsPath

data FollowSymlinks a =
    -- | Recurse into symlinked directories
    FollowSymlinks
  | -- | Do not recurse into symlinked directories, but possibly report them.
    ReportSymlinks (AbsDir -> RelDir -> IO (Maybe a))

newtype AbsDir  = AbsDir  { unAbsDir  :: OsPath }
newtype RelDir  = RelDir  { unRelDir  :: OsPath }
newtype AbsFile = AbsFile { unAbsFile :: OsPath }
newtype RelFile = RelFile { unRelFile :: OsPath }

instance Pretty AbsDir  where pretty = ppShow . unAbsDir
instance Pretty RelDir  where pretty = ppShow . unRelDir
instance Pretty AbsFile where pretty = ppShow . unAbsFile
instance Pretty RelFile where pretty = ppShow . unRelFile

findAllCollect
  :: (RelFile -> Bool)
  -> NonEmpty AbsDir
  -> IO [AbsFile]
findAllCollect p dirs = do
  results <- newIORef []
  let consume :: AbsFile -> IO ()
      consume path = atomicModifyIORef' results ((, ()) . (path :))

  findRec
    FollowSymlinks
    0
    (\_ _ -> True)
    (\_absDir absFile relFile -> pure $ if p relFile then Just absFile else Nothing)
    consume
    dirs
  readIORef results

{-# INLINE findRec #-}
findRec
  :: forall a f ff. (HasCallStack, Foldable f, Foldable ff)
  => FollowSymlinks a
  -> Int                        -- ^ Extra search threads to run in parallel.
  -> (AbsDir -> RelDir -> Bool) -- ^ Whether to visit a directory.
  -> (AbsDir -> AbsFile -> RelFile -> IO (f a))
                                -- ^ What to do with a file. Receives original directory it was located in.
  -> (a -> IO ())               -- ^ Consume output
  -> ff AbsDir                  -- ^ Where to start search.
  -> IO ()
findRec followSymlinks extraJobs dirPred filePred consumeOutput roots = do
  sem <- newNBSem extraJobs
  let runWithRoot :: AbsDir -> IO () -> IO ()
      runWithRoot currRoot goNext = doDir currRoot (coerce takeFileName currRoot) goNext
        where
          currRootWithTrailingSep :: AbsDir
          currRootWithTrailingSep = AbsDir $ addTrailingPathSeparator $ unAbsDir currRoot

          doDir :: AbsDir -> RelDir -> IO () -> IO ()
          doDir absPath relPath processNextDir
            | dirPred absPath relPath = do
              acquired <- tryAcquireNBSem sem
              if acquired
              then
                withAsync (goDirRelease absPath) $ \yAsync ->
                  processNextDir *> wait yAsync
              else
                goDir absPath *> processNextDir
            | otherwise =
              processNextDir

          goDir :: AbsDir -> IO ()
          goDir d =
            bracket
              (Streaming.openDirStream (unAbsDir d))
              Streaming.closeDirStream
              (goDirStream d)

          goDirRelease :: AbsDir -> IO ()
          goDirRelease d =
            bracket
              (Streaming.openDirStream (unAbsDir d))
              (\stream -> Streaming.closeDirStream stream *> releaseNBSem sem)
              (goDirStream d)

          goDirStream :: AbsDir -> Streaming.DirStream -> IO ()
          goDirStream (AbsDir root) stream = go
            where
              go :: IO ()
              go = do
                x <- Streaming.readDirStream stream
                case x of
                  Nothing -> pure ()
                  Just y  -> do
                    let y' :: OsPath
                        y' = root </> y
                    ft <- Streaming.getFileType y'
                    case ft of
                      Streaming.Other        -> go
                      Streaming.File         -> doFile (AbsFile y') (RelFile y) *> go
                      Streaming.FileSym      -> doFile (AbsFile y') (RelFile y) *> go
                      Streaming.Directory    -> doDir (AbsDir y') (RelDir y) go
                      Streaming.DirectorySym ->
                        case followSymlinks of
                          FollowSymlinks        -> doDir (AbsDir y') (RelDir y) go
                          ReportSymlinks report ->
                            traverse_ consumeOutput =<< report (AbsDir y') (RelDir y)

          doFile :: AbsFile -> RelFile -> IO ()
          doFile absPath relPath =
            traverse_ consumeOutput =<< filePred currRootWithTrailingSep absPath relPath

  foldr runWithRoot (pure ()) roots
