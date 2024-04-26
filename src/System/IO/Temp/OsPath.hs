-- |
-- Module:     System.IO.Temp.OsPath
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# OPTIONS_GHC -Wno-deprecations #-}

module System.IO.Temp.OsPath
  ( withSystemTempDirectory
  , getCanonicalTemporaryDirectory
  , createTempDirectory
  , withSystemTempFileContents
  , createTmpDir
  , createTmpFile
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import System.Directory.OsPath (removeFile)
import System.IO (Handle, hClose, openBinaryTempFile)
import System.IO.Temp qualified
import System.OsPath

withSystemTempFileContents
  :: (MonadIO m, MonadMask m)
  => OsString
  -> (Handle -> m ())   -- ^ Initialize file.
  -> (OsPath -> m a) -- ^ Continue with file initialized and handle closed.
  -> m a
withSystemTempFileContents template initialise k = do
  template' <- decodeUtf template
  System.IO.Temp.withSystemTempFile template' $ \path h -> do
    initialise h
    liftIO $ hClose h
    k =<< encodeUtf path

withSystemTempDirectory
  :: (MonadIO m, MonadMask m)
  => OsString        -- ^ Directory name template
  -> (OsPath -> m a) -- ^ Callback that can use the directory
  -> m a
withSystemTempDirectory template k = do
  template' <- decodeUtf template
  System.IO.Temp.withSystemTempDirectory template' (k <=< encodeUtf)

createTempDirectory
  :: OsPath -- ^ Parent directory to create the directory in
  -> OsString -- ^ Directory name template
  -> IO OsPath
createTempDirectory parent template = do
  parent'   <- decodeUtf parent
  template' <- decodeUtf template
  encodeUtf =<< System.IO.Temp.createTempDirectory parent' template'

createTempFile
  :: OsPath -- ^ Parent directory to create the directory in
  -> OsString -- ^ Directory name template
  -> IO (OsPath, Handle)
createTempFile parent template = do
  parent'   <- decodeUtf parent
  template' <- decodeUtf template
  (path, h) <- openBinaryTempFile parent' template'
  (, h) <$> encodeUtf path

getCanonicalTemporaryDirectory :: (MonadThrow m, MonadIO m) => m OsPath
getCanonicalTemporaryDirectory =
  encodeUtf =<< liftIO System.IO.Temp.getCanonicalTemporaryDirectory

createTmpDir :: OsString -> (OsPath -> IO a) -> IO a
createTmpDir template k = do
  tmpDir <- getCanonicalTemporaryDirectory
  k =<< createTempDirectory tmpDir template

createTmpFile
  :: (MonadIO m, MonadMask m)
  => OsString
  -> (Handle -> m ())
  -> (OsPath -> m a)
  -> m a
createTmpFile template initialise k = do
  tmpDir    <- getCanonicalTemporaryDirectory
  (path, h) <- liftIO $ createTempFile tmpDir template
  (initialise h `finally` liftIO (hClose h)) `catch` (\SomeException{} -> liftIO $ removeFile path)
  k path

