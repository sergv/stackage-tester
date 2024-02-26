-- |
-- Module:     StackageTester
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module StackageTester (main) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Data.Bifunctor
import Data.Bitraversable
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Void
import GHC.Stack
import Options.Applicative
import Prettyprinter
import Prettyprinter.Combinators
import Prettyprinter.Generics
import Prettyprinter.Show
import System.Directory.OsPath
import System.Exit
import System.File.OsPath
import System.OsPath as OsPath
import System.OsString as OsString
import System.Process.Typed
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Options qualified as Tasty
import Test.Tasty.Runners qualified as Tasty

data Config = Config
  { cfgWorkDir        :: !OsPath
  , cfgTestResultsDir :: !OsPath
  }

parseConfig :: Parser Config
parseConfig = do
  cfgWorkDir         <- option (eitherReader (bimap show id . OsPath.encodeUtf)) $
    long "work-dir" <>
    metavar "DIR" <>
    value defaultWorkDir <>
    showDefault <>
    help "Path to directory to store all the build files in"

  cfgTestResultsDir  <- option (eitherReader (bimap show id . OsPath.encodeUtf)) $
    long "test-results-dir" <>
    metavar "DIR" <>
    value (defaultWorkDir </> [osp|results|]) <>
    showDefault <>
    help "Directory to store all the build files in"

  pure Config{..}

  where
    defaultWorkDir = [osp|_work|]

data FullConfig = FullConfig
  { fcfgCabalConfigFile :: !OsPath
  , fcfgSubconfig       :: !Config
  , fcfgLimitTests      :: !(Maybe Int)
  , fcfgTastyOpts       :: !Tasty.OptionSet
  }

optsParser :: Parser Tasty.OptionSet -> Parser FullConfig
optsParser tastyParser = do

  fcfgCabalConfigFile <- argument (eitherReader (bimap show id . OsPath.encodeUtf)) $
    metavar "FILE" <>
    help "Path to cabal.config with constraints; formatted like https://www.stackage.org/nightly-2023-10-04/cabal.config"

  fcfgSubconfig <- parseConfig

  fcfgLimitTests <- optional $ option auto $
    long "limit-tests" <>
    help "Limit number of tests to this amount"

  fcfgTastyOpts <- tastyParser

  pure FullConfig{..}

progInfo :: Parser Tasty.OptionSet -> ParserInfo FullConfig
progInfo tastyParser = info
  (helper <*> optsParser tastyParser)
  (fullDesc <> header "Build stackage LTS snapshot and run tests.")

main :: IO ()
main = do

  Tasty.installSignalHandlers

  let ingredients = defaultIngredients
      tastyParser = snd $ Tasty.suiteOptionParser ingredients $ testGroup "" []

  FullConfig{fcfgCabalConfigFile, fcfgSubconfig, fcfgLimitTests, fcfgTastyOpts} <-
    customExecParser (prefs (showHelpOnEmpty <> noBacktrack <> multiSuffix "*")) (progInfo tastyParser)

  pkgs <- parseCabalConfig . T.decodeUtf8 <$> readFile' fcfgCabalConfigFile

  case pkgs of
    [] -> die $ renderString $
      "Didn't extract any packages from cabal config at" <+> ppShow fcfgCabalConfigFile <> ". Is it valid?"
    _  -> pure ()

  let allTests = testGroup "Tests" $ map (mkTest fcfgSubconfig) $ maybe id take fcfgLimitTests pkgs

  createDirectoryIfMissing True $ cfgWorkDir fcfgSubconfig
  createDirectoryIfMissing True $ cfgTestResultsDir fcfgSubconfig

  case Tasty.tryIngredients ingredients fcfgTastyOpts allTests of
    Nothing  ->
      die "No ingredients agreed to run. Something is wrong either with your ingredient set or the options."
    Just act -> do
      ok <- act
      if ok then exitSuccess else exitFailure

  pure ()

data Package = Package
  { pkgName    :: !Text
  , pkgVersion :: !Text
  }
  deriving stock (Show, Generic)
  deriving Pretty via PPGeneric Package

-- data LocatedDoc = LocatedDoc
--   { ldMsg       :: !(Doc Void)
--   , ldCallStack :: CallStack
--   }
--
-- instance Pretty LocatedDoc where
--   pretty (LocatedDoc msg stack) = vacuous msg ## ppCallStack stack

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

cabalBuildFlags :: [String]
cabalBuildFlags =
  [ "--enable-shared"
  , "--enable-executable-dynamic"
  , "--disable-static"
  , "--disable-executable-static"
  , "--disable-profiling"
  , "--disable-documentation"
  , "--enable-tests"
  , "--allow-newer"
  ]

mkTest :: HasCallStack => Config -> Package -> TestTree
mkTest Config{cfgWorkDir, cfgTestResultsDir} Package{pkgName, pkgVersion} =
  testCaseSteps fullPkgName $ \step -> do
    (fullPkgName' :: OsPath) <- OsString.encodeUtf fullPkgName
    let pkgDir :: OsPath
        pkgDir = cfgWorkDir </> fullPkgName'

        logDir = cfgWorkDir </> [osp|logs|]
    alreadyDownloaded <- doesDirectoryExist pkgDir
    pkgDir' <- OsPath.decodeUtf pkgDir
    unless alreadyDownloaded $ do
      step "Unpack"
      runProc Nothing "cabal" ["get", "--destdir", pkgDir'] $ \exitCode stdOut stdErr ->
        "Unpacking of" <+> pretty fullPkgName <+> "failed with exit code" <+> pretty exitCode ## vcat
          [ "Stdout:" ## pretty stdOut
          , "Stderr:" ## pretty stdErr
          ]
    do
      step "Build"
      let buildLog :: OsPath
          buildLog = logDir </> [osp|build-|] <> fullPkgName' <.> [osstr|log|]
      haveCabalProject <- doesFileExist $ pkgDir </> [osp|cabal.project|]
      unless haveCabalProject $ do
        pkgDir </> [osp|cabal.project.local|]
        T.writeFile
        -- createFileLink from to

      buildLog' <- OsPath.decodeUtf buildLog
      runProc (Just pkgDir') "cabal" (["build", "--project-dir", ".", "--build-log", buildLog'] ++ cabalBuildFlags) $ \exitCode stdOut stdErr ->
        "Build of" <+> pretty fullPkgName <+> "failed with exit code" <+> pretty exitCode <> ", check logs at" <+> ppShow buildLog ## vcat
          [ "Stdout:" ## pretty stdOut
          , "Stderr:" ## pretty stdErr
          ]
    step "Test"
    pure ()

  where
    fullPkgName :: String
    fullPkgName = T.unpack $ pkgName <> "-" <> pkgVersion

runProc :: Maybe FilePath -> String -> [String] -> (Int -> Text -> Text -> Doc ann) -> IO ()
runProc cwd cmd args msgOnError = do
  let p = setStdin closed
        $ setStdout byteStringOutput
        $ setStderr byteStringOutput
        $ maybe id setWorkingDir cwd
        $ proc cmd args

  withProcessWait p $ \p' -> do
    let decode = evaluate . TL.toStrict . TL.decodeUtf8
    (!stdOut, !stdErr) <- bitraverse decode decode =<<
      concurrently (atomically (getStdout p')) (atomically (getStderr p'))
    exitCode <- waitExitCode p'
    case exitCode of
      ExitFailure x -> do
        assertFailure $ renderString $ msgOnError x stdOut stdErr
      ExitSuccess   -> pure ()
