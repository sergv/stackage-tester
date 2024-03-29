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
  ) where

-- import Control.Monad
import Control.Monad
import Control.Monad.Catch qualified as MC
import Control.Monad.IO.Class
import System.IO.Temp qualified
import System.OsPath

withSystemTempDirectory
  :: (MonadIO m, MC.MonadMask m)
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

getCanonicalTemporaryDirectory :: IO OsPath
getCanonicalTemporaryDirectory =
  encodeUtf =<< System.IO.Temp.getCanonicalTemporaryDirectory
