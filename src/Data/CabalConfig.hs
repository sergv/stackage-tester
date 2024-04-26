-- |
-- Module:     Data.CabalConfig
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Data.CabalConfig
  ( RawCabalConfig(..)
  , FullyPinnedCabalConfig
  , unFullyPinnedCabalConfig
  , Package(..)
  , parseCabalConfigIO
  , getFullPkgName
  , withFullyPinnedCabalConfig
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Prettyprinter.Generics
import Prettyprinter.Show
import System.File.OsPath
import System.IO (Handle)
import System.IO.Temp.OsPath
import System.OsPath as OsPath
import System.OsString (osstr)

-- | cabal.config taken directly from stackage, e.g. https://www.stackage.org/lts-22.16/cabal.config
newtype RawCabalConfig = RawCabalConfig { unRawCabalConfig :: OsPath }
  deriving (Eq, Ord, Show)

instance Pretty RawCabalConfig where
  pretty = ppShow

-- | Transformed 'RawCabalConfig' where each line like
--
-- > abstract-deque-tests ==0.3,
--
-- Is replaced by
--
-- > any.abstract-deque-tests ==0.3,
--
-- to make snapshot have effect for all Setup dependencies as well as
-- regular dependencies. Otherwise Setups could pick up unsupported
-- configurations and fail to build (it happens in practice).
newtype FullyPinnedCabalConfig = FullyPinnedCabalConfig { unFullyPinnedCabalConfig :: OsPath }
  deriving (Eq, Ord, Show)

instance Pretty FullyPinnedCabalConfig where
  pretty = ppShow

withFullyPinnedCabalConfig
  :: forall m a. (MonadIO m, MonadMask m)
  => Bool
  -> [Package]
  -> (FullyPinnedCabalConfig -> m a)
  -> m a
withFullyPinnedCabalConfig keepTempFile pkgs k =
  (if keepTempFile then createTmpFile else withSystemTempFileContents) [osstr|fully-pinned-cabal.config|] (liftIO . initialise) (k . FullyPinnedCabalConfig)
  where
    initialise :: Handle -> IO ()
    initialise h = do
      T.hPutStrLn h "constraints:"
      for_ pkgs $ \Package{pkgName, pkgVersion} -> do
        let version
              | pkgVersion == "installed"
              = pkgVersion
              | otherwise
              = " ==" <> pkgVersion
        T.hPutStrLn h $ "  , any." <> pkgName <> version

data Package = Package
  { pkgName    :: !Text
  , pkgVersion :: !Text
  }
  deriving stock (Show, Generic)
  deriving Pretty via PPGeneric Package

getFullPkgName :: Package -> Text
getFullPkgName Package{pkgName, pkgVersion} = pkgName <> "-" <> pkgVersion

parseCabalConfigIO :: RawCabalConfig -> IO [Package]
parseCabalConfigIO (RawCabalConfig path) = do
  parseCabalConfig . T.decodeUtf8 <$> readFile' path

-- parseCabalConfig :: forall m. (HasCallStack, MonadError LocatedDoc m) => Text -> m [Package]
parseCabalConfig :: Text -> [Package]
parseCabalConfig = go [] . T.lines
  where
    go :: [Package] -> [Text] -> [Package]
    go acc []
      = reverse acc
    go acc (x : xs)
      | Just x' <- T.stripPrefix "constraints:" x
      = go' acc (x' : xs)
      | otherwise
      = go acc xs

    go' :: [Package] -> [Text] -> [Package]
    go' acc []       = reverse acc
    go' acc (x : xs) = go' (pkg : acc) xs
      where
        x'              = T.dropWhileEnd (== ',') $ T.strip x
        (pkgName, rest) = T.span (/= ' ') x'
        pkg             = Package
          { pkgName
          , pkgVersion = T.dropWhile (\c -> c == ' ' || c == '=') rest
          }

