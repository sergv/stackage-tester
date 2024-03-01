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

{-# OPTIONS_GHC -Wno-deprecations #-}

module StackageTester (main) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Bifunctor
import Data.Bitraversable
import Data.ByteString.Char8 qualified as C8
import Data.Filesystem
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import GHC.Stack
import Options.Applicative
import Prettyprinter
import Prettyprinter.Combinators
import Prettyprinter.Generics
import Prettyprinter.Show
import System.Directory.OsPath
import System.Exit
import System.File.OsPath
import System.IO (IOMode(..), Handle, hClose)
import System.OsPath as OsPath
import System.OsPath.Ext
import System.OsString as OsString
import System.Process.Typed
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Options qualified as Tasty
import Test.Tasty.Runners qualified as Tasty

data Config = Config
  { cfgWorkDir               :: !OsPath
  , cfgCabalConfigFile       :: !OsPath
  , cfgExtraCabalConfigFiles :: ![OsPath]
  }

parseConfig :: Parser Config
parseConfig = do
  cfgWorkDir         <- option (eitherReader (bimap show id . OsPath.encodeUtf)) $
    long "work-dir" <>
    metavar "DIR" <>
    value defaultWorkDir <>
    showDefault <>
    help "Path to directory to store all the build files in"

  cfgCabalConfigFile <- option (eitherReader (bimap show id . OsPath.encodeUtf)) $
    long "cabal-config-main" <>
    metavar "FILE" <>
    help "Path to cabal.config with constraints; formatted like https://www.stackage.org/nightly-2023-10-04/cabal.config"

  cfgExtraCabalConfigFiles <- many $ option (eitherReader (bimap show id . OsPath.encodeUtf)) $
    long "extra-cabal-config" <>
    metavar "FILE" <>
    help "Path to cabal.config with auxiliary build options to take effect during builds"

  pure Config{..}

  where
    defaultWorkDir = [osp|_work|]

data FullConfig = FullConfig
  { fcfgSubconfig       :: !Config
  , fcfgLimitTests      :: !(Maybe Int)
  , fcfgTastyOpts       :: !Tasty.OptionSet
  }

optsParser :: Parser Tasty.OptionSet -> Parser FullConfig
optsParser tastyParser = do

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

resolveRelativePaths :: Config -> IO Config
resolveRelativePaths Config{cfgWorkDir, cfgCabalConfigFile, cfgExtraCabalConfigFiles} = do
  cfgWorkDir'               <- makeAbsolute cfgWorkDir
  cfgCabalConfigFile'       <- makeAbsolute cfgCabalConfigFile
  cfgExtraCabalConfigFiles' <- traverse makeAbsolute cfgExtraCabalConfigFiles
  pure Config
    { cfgWorkDir               = cfgWorkDir'
    , cfgCabalConfigFile       = cfgCabalConfigFile'
    , cfgExtraCabalConfigFiles = cfgExtraCabalConfigFiles'
    }

main :: IO ()
main = do

  Tasty.installSignalHandlers

  let ingredients = defaultIngredients
      tastyParser = snd $ Tasty.suiteOptionParser ingredients $ testGroup "" []

  FullConfig{fcfgSubconfig, fcfgLimitTests, fcfgTastyOpts} <-
    customExecParser (prefs (showHelpOnEmpty <> noBacktrack <> multiSuffix "*")) (progInfo tastyParser)

  fcfgSubconfig' <- resolveRelativePaths fcfgSubconfig

  let cabalConfigPath = cfgCabalConfigFile fcfgSubconfig'

  pkgs <- parseCabalConfig . T.decodeUtf8 <$> readFile' cabalConfigPath

  case pkgs of
    [] -> die $ renderString $
      "Didn't extract any packages from cabal config at" <+> ppShow cabalConfigPath <> ". Is it valid?"
    _  -> pure ()

  for_ (cfgExtraCabalConfigFiles fcfgSubconfig') $ \path -> do
    exists <- doesFileExist path
    unless exists $
      die $ renderString $ "Extra cabal config file does not exist:" <+> ppShow path

  let pkgs'    = filter ((/= "installed") . pkgVersion) pkgs
      allTests = testGroup "Tests" $ map (mkTest fcfgSubconfig') $ maybe id take fcfgLimitTests pkgs'

  let workDir = cfgWorkDir fcfgSubconfig'
  createDirectoryIfMissing True $ workDir
  createDirectoryIfMissing True $ workDir </> [osp|pkgs|]
  createDirectoryIfMissing True $ workDir </> [osp|build-logs|]
  createDirectoryIfMissing True $ workDir </> [osp|test-logs|]

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
  , "--enable-optimization"
  ]

mkTest :: HasCallStack => Config -> Package -> TestTree
mkTest Config{cfgWorkDir, cfgCabalConfigFile, cfgExtraCabalConfigFiles} Package{pkgName, pkgVersion} =
  testCaseSteps fullPkgName $ \step -> do
    (fullPkgName' :: OsPath) <- OsPath.encodeUtf fullPkgName
    let pkgsDir :: OsPath
        pkgsDir = cfgWorkDir </> [osp|pkgs|]

        pkgDir :: OsPath
        pkgDir = pkgsDir </> fullPkgName'

        buildLogDir = cfgWorkDir </> [osp|build-logs|]
        testLogDir  = cfgWorkDir </> [osp|test-logs|]

    alreadyDownloaded <- doesDirectoryExist pkgDir
    unless alreadyDownloaded $ do
      step "Unpack"
      pkgsDir' <- OsPath.decodeUtf pkgsDir
      runProc' Nothing "cabal" ["get", "--destdir", pkgsDir', fullPkgName] $ \exitCode stdOut stdErr ->
        "Unpacking of" <+> pretty fullPkgName <+> "failed with exit code" <+> pretty exitCode ## vcat
          [ "Stdout:" ## pretty stdOut
          , "Stderr:" ## pretty stdErr
          ]

    pkgDir' <- OsPath.decodeUtf pkgDir
    do
      step "Build"
      let buildLog :: OsPath
          buildLog = buildLogDir </> [osstr|build-|] <> fullPkgName' <.> [osstr|log|]

          buildLogTmp = buildLog <> [osstr|.tmp|]

      mkCabalProjectLocal fullPkgNameText (AbsDir pkgDir) cfgCabalConfigFile cfgExtraCabalConfigFiles

      -- buildLog' <- OsPath.decodeUtf buildLog

      (`finally` (doesFileExist buildLogTmp >>= \e -> when e (removeFile buildLogTmp))) $
        withFile buildLogTmp WriteMode $ \buildLogH ->
          runProc buildLogH (Just pkgDir') "cabal" (["build", "--project-dir", ".", "all", "-j2"] ++ cabalBuildFlags)
            (do
              hClose buildLogH
              firstLine <- withFile buildLogTmp ReadMode C8.hGetLine
              unless (firstLine == "Up to date") $
                renameFile buildLogTmp buildLog)
            (\exitCode -> do
              hClose buildLogH
              renameFile buildLogTmp buildLog
              output <- T.decodeUtf8 <$> readFile' buildLog
              pure $
                "Build of" <+> pretty fullPkgName <+> "failed with exit code" <+> pretty exitCode <> ", check logs at" <+> ppShow buildLog ## pretty output)

    do
      step "Test"

      let testLog :: OsPath
          testLog = testLogDir </> [osstr|test-|] <> fullPkgName' <.> [osstr|log|]

      testLog' <- OsPath.decodeUtf testLog
      runProc' (Just pkgDir') "cabal" (["test", "--project-dir", ".", "--test-log", testLog', "--test-show-details=always", "all"] ++ cabalBuildFlags)
        (\exitCode stdOut stdErr ->
          "Test of" <+> pretty fullPkgName <+> "failed with exit code" <+> pretty exitCode <> ", check logs at" <+> ppShow testLog ## vcat
            [ "stdout:" ## pretty stdOut
            , "stderr:" ## pretty stdErr
            ])
    pure ()

  where
    fullPkgName :: String
    fullPkgName = T.unpack fullPkgNameText
    fullPkgNameText :: Text
    fullPkgNameText = pkgName <> "-" <> pkgVersion

mkCabalProjectLocal :: Text -> AbsDir -> OsPath -> [OsPath] -> IO ()
mkCabalProjectLocal fullPkgName pkgDir cabalConfigFile extraConfigs = do
  let importStr :: Text
      importStr = T.unlines $ map (\p -> "import: " <> pathToText p) $ cabalConfigFile : extraConfigs

  haveCabalProject <- doesFileExist $ unAbsDir pkgDir </> [osp|cabal.project|]
  cabalFiles <-
    if haveCabalProject
    then
      pure Nothing
    else do
      cabalFiles <- findAllCollect ((== [osstr|.cabal|]) . takeExtension . unRelFile) $ pkgDir :| []
      case cabalFiles of
        []     -> error $ renderString $ "Package" <+> pretty fullPkgName <+> "has no cabal files"
        x : xs -> pure $ Just $ x :| xs

  writeFile' (unAbsDir pkgDir </> [osp|cabal.project.local|]) $ T.encodeUtf8 $ T.unlines $
    importStr :
    case cabalFiles of
      Nothing -> []
      Just xs -> "packages:" : map (("  " <>) . pathToText . unAbsFile) (toList xs)

runProc
  :: Handle
  -> Maybe FilePath
  -> String
  -> [String]
  -> IO ()
  -> (Int -> IO (Doc ann))
  -> IO ()
runProc out cwd cmd args onSuccess msgOnError = do
  let p = setStdin closed
        $ setStdout (useHandleClose out)
        $ setStderr (useHandleClose out)
        $ maybe id setWorkingDir cwd
        $ proc cmd args

  when debug $
    putStrLn $ "Running " ++ show (cmd : args) ++ " in " ++ show cwd

  withProcessWait p $ \p' -> do
    -- let decode = evaluate . TL.toStrict . TL.decodeUtf8
    -- (!stdOut, !stdErr) <- bitraverse decode decode =<<
    --   concurrently (atomically (getStdout p')) (atomically (getStderr p'))
    -- when debug $ do
    --   unless (T.null stdOut) $
    --     putStrLn $ "stdOut:\n" ++ show stdOut
    --   unless (T.null stdErr) $
    --     putStrLn $ "stdErr:\n" ++ show stdErr
    exitCode <- waitExitCode p'
    case exitCode of
      ExitFailure x ->
        assertFailure . renderString =<< msgOnError x
      ExitSuccess   ->
        onSuccess

runProc'
  :: Maybe FilePath
  -> String
  -> [String]
  -> (Int -> Text -> Text -> Doc ann)
  -> IO ()
runProc' cwd cmd args msgOnError = do
  let p = setStdin closed
        $ setStdout byteStringOutput
        $ setStderr byteStringOutput
        $ maybe id setWorkingDir cwd
        $ proc cmd args

  when debug $
    putStrLn $ "Running " ++ show (cmd : args) ++ " in " ++ show cwd

  withProcessWait p $ \p' -> do
    let decode = evaluate . TL.toStrict . TL.decodeUtf8
    (!stdOut, !stdErr) <- bitraverse decode decode =<<
      concurrently (atomically (getStdout p')) (atomically (getStderr p'))
    when debug $ do
      unless (T.null stdOut) $
        putStrLn $ "stdOut:\n" ++ show stdOut
      unless (T.null stdErr) $
        putStrLn $ "stdErr:\n" ++ show stdErr
    exitCode <- waitExitCode p'
    case exitCode of
      ExitFailure x ->
        assertFailure $ renderString $ msgOnError x stdOut stdErr
      ExitSuccess   -> pure ()


debug :: Bool
debug = False
