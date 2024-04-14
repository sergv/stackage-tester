-- |
-- Module:     Data.Cabal
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Cabal
  ( getCabalComponents
  , ComponentType(..)
  ) where

import Prelude hiding (readFile)

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.List.NonEmpty (toList)
import Data.Maybe
import Data.Version
import System.File.OsPath
import System.OsPath

import Distribution.Compiler (AbiTag(NoAbiTag), CompilerFlavor(GHC), CompilerId(CompilerId), CompilerInfo, unknownCompilerInfo)
import Distribution.Package (pkgName, Dependency(..))
import Distribution.Package (PackageName)
import Distribution.PackageDescription (GenericPackageDescription, PackageDescription(..), exeName)
import Distribution.PackageDescription (allLibraries, libName)
import Distribution.PackageDescription (mkFlagAssignment)
import Distribution.PackageDescription.Configuration (finalizePD)
import Distribution.PackageDescription.Parsec (runParseResult, parseGenericPackageDescription)
import Distribution.Parsec.Error (showPError)
import Distribution.System (buildPlatform)
import Distribution.Types.Benchmark (Benchmark(benchmarkName))
import Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec(..))
import Distribution.Types.ForeignLib (ForeignLib(foreignLibName))
import Distribution.Types.LibraryName (libraryNameString)
import Distribution.Types.TestSuite (TestSuite(testName))
import Distribution.Types.UnqualComponentName
import Distribution.Version qualified as Cabal

data ComponentType
  = CTLibrary
  | CTForeignLibrary
  | CTExecutable
  | CTTestSuite
  | CTBenchmark
  deriving (Eq, Ord, Show, Enum, Bounded)

_componentTypePrefix :: ComponentType -> C8.ByteString
_componentTypePrefix = \case
  CTLibrary        -> "lib"
  CTForeignLibrary -> "flib"
  CTExecutable     -> "exe"
  CTTestSuite      -> "test"
  CTBenchmark      -> "bench"

-- | Gather files and modules that constitute each component.
getComponents
  :: PackageDescription
  -> [(ComponentType, UnqualComponentName)]
getComponents pkgDesc =
  [ (CTLibrary, name)
  | lib <- allLibraries pkgDesc
  , let name = fromMaybe (packageNameToUnqualComponentName packageName) $ libraryNameString $ libName lib
  ] ++
  [ (CTForeignLibrary, foreignLibName flib)
  | flib <- foreignLibs pkgDesc
  ] ++
  [ (CTExecutable, exeName exe)
  | exe <- executables pkgDesc
  ] ++
  [ (CTTestSuite, testName tst)
  | tst <- testSuites pkgDesc
  ] ++
  [ (CTBenchmark, benchmarkName tst)
  | tst <- benchmarks pkgDesc
  ]
  where
    packageName :: PackageName
    packageName = pkgName $ package pkgDesc

getCabalComponents
  :: (MonadError String m, MonadIO m, MonadThrow m)
  => OsPath
  -> m [(ComponentType, UnqualComponentName)]
getCabalComponents configFile = do
  genericDesc <- readGenericPkgDescr configFile
  case getConcretePackageDescription genericDesc of
    Left deps     -> throwError $ "Missing dependencies:\n" ++ unlines (map show deps)
    Right pkgDesc -> pure $ getComponents pkgDesc

readGenericPkgDescr
  :: (MonadError String m, MonadIO m, MonadThrow m)
  => OsPath
  -> m GenericPackageDescription
readGenericPkgDescr path = do
  path' <- decodeUtf path
  contents <- liftIO $ readFile' path
  case parsePkgDescr path' contents of
    Left msgs -> throwError $ "Failed to parse cabal '" ++ path' ++ "':\n" ++ unlines msgs
    Right x   -> pure x

parsePkgDescr :: FilePath -> BS.ByteString -> Either [String] GenericPackageDescription
parsePkgDescr path cabalFileContents =
  case res of
    Left (_version, errs) -> Left $ map (showPError path) $ toList errs
    Right x               -> pure x
  where
    (_warnings, res) = runParseResult $ parseGenericPackageDescription cabalFileContents

getConcretePackageDescription
  :: GenericPackageDescription
  -> Either [Dependency] PackageDescription
getConcretePackageDescription genericDesc = do
  let enabled :: ComponentRequestedSpec
      enabled = ComponentRequestedSpec
        { testsRequested      = True
        , benchmarksRequested = True
        }
  fst <$> finalizePD
    (mkFlagAssignment []) -- Flag assignment
    enabled               -- Enable all components
    (const True)          -- Whether given dependency is available
    buildPlatform
    buildCompilerId
    []                    -- Additional constraints
    genericDesc

buildCompilerId :: CompilerInfo
buildCompilerId = unknownCompilerInfo compId NoAbiTag
  where
    compId :: CompilerId
    compId = CompilerId GHC compVersion
    compVersion :: Cabal.Version
    compVersion = Cabal.mkVersion' $ makeVersion [9, 8, 2] -- compilerVersion
