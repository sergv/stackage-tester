-- |
-- Module:     StackageTester
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
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
import Control.Monad.Except
import Data.Bifunctor
import Data.Bitraversable
import Data.ByteString.Char8 qualified as C8
import Data.Cabal
import Data.CabalConfig
import Data.Coerce (coerce)
import Data.Filesystem
import Data.Foldable
import Data.List qualified as L
import Data.Lock (Lock)
import Data.Lock qualified as Lock
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Distribution.Types.UnqualComponentName
import GHC.Stack
import Options.Applicative hiding (str)
import Prettyprinter hiding (dquote)
import Prettyprinter.Combinators
import Prettyprinter.Show
import System.Directory.OsPath
import System.Exit
import System.File.OsPath
import System.FilePath qualified as FilePath
import System.IO (IOMode(..), Handle, hClose)
import System.IO.Temp.OsPath
import System.OsPath as OsPath
import System.OsPath.Ext
import System.OsString (osstr)
import System.Process.Typed
import Test.Tasty hiding (after)
import Test.Tasty.HUnit
import Test.Tasty.Options qualified as Tasty
import Test.Tasty.Runners qualified as Tasty

data Config = Config
  { cfgCabalConfigFile         :: !RawCabalConfig
  , cfgExtraCabalConfigFiles   :: ![OsPath]
  , cfgGhcExe                  :: !(Maybe String)
  , cfgCabalExe                :: !String
  , cfgKeepTempArtifacts       :: !Bool
  , cfgSkipDeps                :: !Bool
  , cfgStickToStackagePackages :: !Bool
  }

parseConfig :: Parser Config
parseConfig = do
  cfgCabalConfigFile <- option (eitherReader (bimap show RawCabalConfig . OsPath.encodeUtf)) $
    long "cabal-config-main" <>
    metavar "FILE" <>
    help "Path to cabal.config with version constraints; formatted like https://www.stackage.org/nightly-2023-10-04/cabal.config"

  cfgExtraCabalConfigFiles <- many $ option (eitherReader (bimap show id . OsPath.encodeUtf)) $
    long "extra-cabal-config" <>
    metavar "FILE" <>
    help "Path to cabal.config with auxiliary build options to take effect during builds"

  cfgGhcExe <- optional $ strOption $
    long "with-ghc" <>
    metavar "PATH" <>
    help "ghc executable to use"

  cfgCabalExe <- strOption $
    long "with-cabal" <>
    metavar "PATH" <>
    value "cabal" <>
    showDefault <>
    help "cabal executable to use"

  cfgKeepTempArtifacts <- switch $
    long "keep-tmp" <>
    help "Don't remove working directories and other temporary files where packages are built for further inspection and debugging"

  cfgSkipDeps <- switch $
    long "skip-deps" <>
    help "Skip building dependencies, use with caution"

  cfgStickToStackagePackages <- switch $
    long "stick-to-stackage-packages" <>
    help "Only use packages from stackage snapshot disallowing any other packages (modulo bugs in the tool). I.e. if not provided then --cabal-config-main will just supply package names and versions will be taken the latest possible (will download the latest but during build will stick to constraints imposed by individual packages)"

  pure Config{..}

data FullConfig = FullConfig
  { fcfgSubconfig  :: !Config
  , fcfgLogsDir    :: !OsPath
  , fcfgLimitTests :: !(Maybe Int)
  , fcfgTastyOpts  :: !Tasty.OptionSet
  }

optsParser :: Parser Tasty.OptionSet -> Parser FullConfig
optsParser tastyParser = do
  fcfgSubconfig <- parseConfig

  fcfgLogsDir   <- option (eitherReader (bimap show id . OsPath.encodeUtf)) $
    long "logs-dir" <>
    metavar "DIR" <>
    value defaultLogsDir <>
    showDefault <>
    help "Path to directory to store all the build files in"

  fcfgLimitTests <- optional $ option auto $
    long "limit-tests" <>
    help "Limit number of tests to this amount"

  fcfgTastyOpts <- tastyParser

  pure FullConfig{..}
  where
    defaultLogsDir = [osp|_logs|]

progInfo :: Parser Tasty.OptionSet -> ParserInfo FullConfig
progInfo tastyParser = info
  (helper <*> optsParser tastyParser)
  (fullDesc <> header "Build stackage LTS snapshot and run tests.")

resolveRelativePaths :: FullConfig -> IO FullConfig
resolveRelativePaths fcfg@FullConfig{fcfgSubconfig, fcfgLogsDir} = do
  fcfgLogsDir'   <- makeAbsolute fcfgLogsDir
  fcfgSubconfig' <- resolveRelativePaths' fcfgSubconfig
  pure fcfg
    { fcfgSubconfig = fcfgSubconfig'
    , fcfgLogsDir   = fcfgLogsDir'
    }
  where
    resolveRelativePaths' :: Config -> IO Config
    resolveRelativePaths' cfg@Config{cfgCabalConfigFile, cfgExtraCabalConfigFiles} = do
      cfgCabalConfigFile'       <- coerce makeAbsolute cfgCabalConfigFile
      cfgExtraCabalConfigFiles' <- traverse makeAbsolute cfgExtraCabalConfigFiles
      pure cfg
        { cfgCabalConfigFile       = cfgCabalConfigFile'
        , cfgExtraCabalConfigFiles = cfgExtraCabalConfigFiles'
        }

data DirConfig = DirConfig
  { dcLogsDir             :: !OsPath
  , dcBuildLogsSuccessDir :: !OsPath
  , dcBuildLogsFailedDir  :: !OsPath
  , dcTestLogsSuccessDir  :: !OsPath
  , dcTestLogsFailedDir   :: !OsPath
  }

withDirs :: OsPath -> (DirConfig -> IO a) -> IO a
withDirs logsDir k = do
  let dcBuildLogsSuccessDir = logsDir </> [osp|build-logs|]
      dcBuildLogsFailedDir  = logsDir </> [osp|build-logs-failed|]
      dcTestLogsSuccessDir  = logsDir </> [osp|test-logs|]
      dcTestLogsFailedDir   = logsDir </> [osp|test-logs-failed|]

  createDirectoryIfMissing True logsDir
  createDirectoryIfMissing True dcBuildLogsSuccessDir
  createDirectoryIfMissing True dcBuildLogsFailedDir
  createDirectoryIfMissing True dcTestLogsSuccessDir
  createDirectoryIfMissing True dcTestLogsFailedDir

  k $ DirConfig
    { dcLogsDir = logsDir
    , dcBuildLogsSuccessDir
    , dcBuildLogsFailedDir
    , dcTestLogsSuccessDir
    , dcTestLogsFailedDir
    }

reportErrorWithStdoutAndStderr :: Doc ann -> (Int -> Text -> Text -> Doc ann)
reportErrorWithStdoutAndStderr msg _ stdOut stdErr =
  msg ## vcat
    [ "Stdout:" ## pretty stdOut
    , "Stderr:" ## pretty stdErr
    ]

main :: IO ()
main = do

  Tasty.installSignalHandlers

  let ingredients = defaultIngredients
      tastyParser = snd $ Tasty.suiteOptionParser ingredients $ testGroup "" []

  FullConfig{fcfgSubconfig, fcfgLogsDir, fcfgLimitTests, fcfgTastyOpts} <-
    resolveRelativePaths =<< customExecParser (prefs (showHelpOnEmpty <> noBacktrack <> multiSuffix "*")) (progInfo tastyParser)

  findExecutable [osstr|ghci|] >>= \case
    Nothing -> die $ renderString "Cannot find ‘ghc’ executable in PATH, it’s required for ghcid tests."
    Just _  -> pure ()

  findExecutable [osstr|ghc|] >>= \case
    Nothing   -> die $ renderString "Cannot find ‘ghc’ executable in PATH, it’s required for alex tests."
    Just path -> do
      case cfgGhcExe fcfgSubconfig of
        Nothing           -> pure ()
        Just specifiedExe -> do
          specifiedVersion <-
            runProcCaptureOutput Nothing specifiedExe ["--version"] (\stdOut _stdErr -> pure stdOut)
              (reportErrorWithStdoutAndStderr ("Failed to get version of specified ghc executable" <+> pretty specifiedExe <> ":"))
          path' <- OsPath.decodeUtf path
          defaultVersion <-
            runProcCaptureOutput Nothing path' ["--version"] (\stdOut _stdErr -> pure stdOut)
              (reportErrorWithStdoutAndStderr ("Failed to get version of specified ghc executable" <+> pretty specifiedExe <> ":"))
          unless (specifiedVersion == defaultVersion) $
            die $ renderString $ ppDictHeader
              "Default ghc executable and specified ghc executable have different verisons"
              [ "default ‘ghc’ version" :-> ppShow defaultVersion
              , "‘--with-ghc’ version"  :-> ppShow specifiedVersion
              ]
          pure ()

  for_ [([osstr|hspec-discover|], "hspec-discover"), ([osstr|tasty-discover|], "tasty-discover"), ([osstr|doctest-driver-gen|], "doctest-driver-gen"), ([osstr|htfpp|], "HTF")] $ \(exe, (pkg :: Doc ann)) ->
    findExecutable exe >>= \case
      Nothing -> die $ renderString $ "Cannot find" <+> ppShow exe <+> "executable in PATH, it’s required for building some packages. Try ‘cabal install" <+> pkg <> "’"
      Just{}  -> pure ()

  let cabalConfigPath = cfgCabalConfigFile fcfgSubconfig

  pkgs <- parseCabalConfigIO cabalConfigPath

  let pkgs' = filter ((/= "installed") . pkgVersion) pkgs

  case pkgs' of
    [] -> die $ renderString $
      "Didn't extract any packages from cabal config at" <+> ppShow cabalConfigPath <> ". Is it valid?"
    _  -> pure ()

  for_ (cfgExtraCabalConfigFiles fcfgSubconfig) $ \path -> do
    exists <- doesFileExist path
    unless exists $
      die $ renderString $ "Extra cabal config file does not exist:" <+> ppShow path

  lock <- Lock.new

  let withMaybeCabalConfig k
        | cfgStickToStackagePackages fcfgSubconfig
        = withFullyPinnedCabalConfig (cfgKeepTempArtifacts fcfgSubconfig) pkgs (k . Just)
        | otherwise
        = k Nothing

  withMaybeCabalConfig $ \fullyPinnedConfigPath -> do

    withDirs fcfgLogsDir $ \dirs -> do
      let allTests = testGroup "Tests" $ map (mkTest lock dirs fcfgSubconfig fullyPinnedConfigPath) $ maybe id take fcfgLimitTests pkgs'

      case Tasty.tryIngredients ingredients fcfgTastyOpts allTests of
        Nothing  ->
          die "No ingredients agreed to run. Something is wrong either with your ingredient set or the options."
        Just act -> do
          ok <- act
          if ok then exitSuccess else exitFailure

      pure ()

-- data LocatedDoc = LocatedDoc
--   { ldMsg       :: !(Doc Void)
--   , ldCallStack :: CallStack
--   }
--
-- instance Pretty LocatedDoc where
--   pretty (LocatedDoc msg stack) = vacuous msg ## ppCallStack stack

data EnableTests = EnableTests | SkipTests

cabalBuildFlags :: EnableTests -> [String]
cabalBuildFlags enableTests =
  [ "--enable-shared"
  , "--enable-executable-dynamic"
  , "--disable-static"
  , "--disable-executable-static"
  , "--disable-profiling"
  , "--disable-documentation"
  , "--enable-optimization"
  , "--remote-build-reporting=none"
  , "--enable-executable-stripping"
  , "--enable-library-stripping"
  ] ++ testFlags
  where
    testFlags :: [String]
    testFlags = case enableTests of
      EnableTests -> ["--enable-tests"]
      SkipTests   -> []

-- "downloading blah blah\nblah\nUnpacking to\n/tmp/workdir-type-of-html-static-1a7ec6c8f1a80449/type-of-html-static-0.1.0.2/\n"
-- "Unpacking to\n/tmp/workdir-type-of-html-static-1a7ec6c8f1a80449/type-of-html-static-0.1.0.2/\n"
-- "Unpacking to /tmp/workdir-abstract-deque-a87ecf27dac68151/abstract-deque-0.3/\n"
findUnpackDir :: Text -> Maybe Text
findUnpackDir output =
  case T.splitOn "Unpacking to" output of
    _before : after : [] -> Just $ T.dropWhileEnd FilePath.isPathSeparator $ T.strip after
    _ -> Nothing

mkTest :: HasCallStack => Lock "deps-build" -> DirConfig -> Config -> Maybe FullyPinnedCabalConfig -> Package -> TestTree
mkTest
  buildDepsLock
  DirConfig{dcLogsDir, dcBuildLogsSuccessDir, dcBuildLogsFailedDir, dcTestLogsSuccessDir, dcTestLogsFailedDir}
  Config{cfgExtraCabalConfigFiles, cfgGhcExe, cfgCabalExe, cfgKeepTempArtifacts, cfgSkipDeps, cfgStickToStackagePackages}
  cabalConfigPath
  pkg =
  testCaseSteps (T.unpack (pkgName pkg)) $ \step -> do

    (pkgName' :: OsPath) <- OsPath.encodeUtf $ T.unpack $ pkgName pkg

    let tmpWorkDir = ([osp|workdir-|] <> pkgName')
        ghcArg     = case cfgGhcExe of
          Nothing -> []
          Just x  -> ["-w", x]
    (if cfgKeepTempArtifacts then createTmpDir tmpWorkDir else withSystemTempDirectory tmpWorkDir) $ \tmpDir -> do

      step "Unpack"
      pkgsDir' <- OsPath.decodeUtf tmpDir

      (pkgDir :: OsPath) <- runProcCaptureOutput
        Nothing
        cfgCabalExe
        ["get", "--destdir", pkgsDir', T.unpack $ pkgName pkg]
        (\stdOut stdErr ->
          case (T.null stdErr, findUnpackDir stdOut) of
            (False, _)           -> assertFailure $ renderString $
              "Unexpected stderr from ‘cabal unpack’:" ## pretty stdErr
            (True,     Just dir) ->
              pure $ pathFromText dir
            (True,     Nothing)  -> assertFailure $ renderString $
              "Unexpected ‘cabal unpack’ output:" ## pretty (show stdOut))
        (\exitCode stdOut stdErr ->
        "Unpacking of" <+> pretty (pkgName pkg) <+> "failed with exit code" <+> pretty exitCode ## vcat
          [ "Stdout:" ## pretty stdOut
          , "Stderr:" ## pretty stdErr
          ])

      let fullPkgName' :: OsPath
          fullPkgName' = L.last $ splitDirectories pkgDir

      (fullPkgName :: String) <- OsPath.decodeUtf fullPkgName'
      -- (fullPkgName' :: OsPath) <- OsPath.encodeUtf fullPkgName

      -- pkgDir'   <- OsPath.decodeUtf pkgDir
      cabalFile <- mkCabalProjectLocal pkg (AbsDir pkgDir) cabalConfigPath cfgExtraCabalConfigFiles

      let runTests
            | pkgName pkg `S.member` packagesSkipTest = SkipTests
            | otherwise                               = EnableTests

          allowNewerArg :: [String]
          allowNewerArg
            | cfgStickToStackagePackages = ["--allow-newer"] -- To combat revisions
            | otherwise                  = []

      unless cfgSkipDeps $ do
        step "Build (--only-deps)"
        Lock.withAcquired buildDepsLock $ do
          let buildLog :: OsPath
              buildLog = [osstr|build-deps-|] <> fullPkgName' <.> [osstr|log|]

              buildLogTmp = dcLogsDir </> buildLog <> [osstr|.tmp|]

          (`finally` removeFileIfExists buildLogTmp) $
            withFile buildLogTmp WriteMode $ \buildLogH ->
              runProc buildLogH (Just pkgDir)
                cfgCabalExe
                (["build"] ++ cabalBuildFlags runTests ++ ghcArg ++ allowNewerArg ++ ["--project-dir", ".", ":all", "-j4", "--only-dependencies"])
                (do
                  firstLine <- withFile buildLogTmp ReadMode C8.hGetLine
                  unless (firstLine == "Up to date") $ do
                    renameFile buildLogTmp (dcBuildLogsSuccessDir </> buildLog))
                (\cmd exitCode -> do
                  let dest = dcBuildLogsFailedDir </> buildLog
                  renameFile buildLogTmp dest
                  output <- T.decodeUtf8 <$> readFile' dest
                  pure $ ppDictHeader ("Build of dependencies of" <+> pretty fullPkgName <+> "failed with exit code" <+> pretty exitCode)
                    [ "Logs location" :-> ppShow dest
                    , "Command"       --> show cmd
                    , "Directory"     :-> ppShow pkgDir
                    , "Output"        --> output
                    ])

      do
        step "Build"
        let buildLog :: OsPath
            buildLog = [osstr|build-|] <> fullPkgName' <.> [osstr|log|]

            buildLogTmp = dcLogsDir </> buildLog <> [osstr|.tmp|]

        (`finally` removeFileIfExists buildLogTmp) $
          withFile buildLogTmp WriteMode $ \buildLogH ->
            runProc buildLogH (Just pkgDir)
              cfgCabalExe
              -- Environment is crucial to make doctests work.
              (["build"] ++ cabalBuildFlags runTests ++ ghcArg ++ allowNewerArg ++ ["--project-dir", ".", ":all", "-j1", "--write-ghc-environment-files=always"])
              (do
                firstLine <- withFile buildLogTmp ReadMode C8.hGetLine
                unless (firstLine == "Up to date") $ do
                  renameFile buildLogTmp (dcBuildLogsSuccessDir </> buildLog))
              (\cmd exitCode -> do
                let dest = dcBuildLogsFailedDir </> buildLog
                renameFile buildLogTmp dest
                output <- T.decodeUtf8 <$> readFile' dest
                pure $ ppDictHeader ("Build of" <+> pretty fullPkgName <+> "failed with exit code" <+> pretty exitCode)
                  [ "Logs location" :-> ppShow dest
                  , "Command"       --> show cmd
                  , "Directory"     :-> ppShow pkgDir
                  , "Output"        --> output
                  ])

      case runTests of
        SkipTests   -> step "Tests skipped"
        EnableTests -> do
          step "Test"

          components <- either assertFailure pure =<< runExceptT (getCabalComponents (unAbsFile cabalFile))

          let tests :: [String]
              tests =
                [ unUnqualComponentName name
                | (CTTestSuite, name) <- components
                ]

          forConcurrently_ tests $ \test -> do

            (test' :: OsPath) <- OsPath.encodeUtf test

            let testLog :: OsPath
                testLog = [osstr|test-|] <> fullPkgName' <.> test' <.> [osstr|log|]

                testLogTmp = dcLogsDir </> testLog <> [osstr|.tmp|]

                onSuccess = do
                  renameFile testLogTmp (dcTestLogsSuccessDir </> testLog)

                onFailure cmd exitCode = do
                  let dest = dcTestLogsFailedDir </> testLog
                  renameFile testLogTmp dest
                  output <- T.decodeUtf8 <$> readFile' dest
                  pure $ ppDictHeader ("Test of" <+> pretty fullPkgName <+> "failed with exit code" <+> pretty exitCode)
                    [ "Logs location" :-> ppShow dest
                    , "Command"       --> show cmd
                    , "Directory"     :-> ppShow pkgDir
                    , "Output"        --> output
                    ]

            (`finally` removeFileIfExists testLogTmp) $
              withFile testLogTmp WriteMode $ \testLogH ->
                runProc testLogH (Just pkgDir)
                  cfgCabalExe
                  (["run", "test:" ++ test] ++ cabalBuildFlags runTests ++ ghcArg ++ allowNewerArg ++ ["--project-dir", "."])
                  onSuccess
                  onFailure

      pure ()

removeFileIfExists :: OsPath -> IO ()
removeFileIfExists path = do
  exists <- doesFileExist path
  when exists $
    removeFile path

mkCabalProjectLocal :: HasCallStack => Package -> AbsDir -> Maybe FullyPinnedCabalConfig -> [OsPath] -> IO AbsFile
mkCabalProjectLocal pkg@Package{pkgName} pkgDir cabalConfigFile extraConfigs = do
  let importStr :: Text
      importStr
        = T.unlines
        $ mapMaybe (\p -> (("import: " <>) . pathToText) <$> p)
        $ (unFullyPinnedCabalConfig <$> cabalConfigFile) : map Just extraConfigs

  haveCabalProject <- doesFileExist $ unAbsDir pkgDir </> [osp|cabal.project|]
  cabalFile        <- do
    toplevelFiles <- listDirectory $ unAbsDir pkgDir
    -- cabalFiles <- findAllCollect ((== (pathFromText pkgName <.> [osstr|cabal|])) . unRelFile) $ pkgDir :| []
    let cabalFile = pathFromText pkgName <.> [osstr|cabal|]
    case L.find ((== cabalFile)) toplevelFiles of
      Nothing -> error $ renderString $ "Could not locate cabal file for package" <+> pretty (getFullPkgName pkg) <+> "in its toplevel directory" <+> pretty pkgDir
      Just x  -> pure $ AbsFile $ unAbsDir pkgDir </> x

  writeFile' (unAbsDir pkgDir </> [osp|cabal.project.local|]) $ T.encodeUtf8 $ T.unlines $
    importStr :
    case (haveCabalProject, cabalFile) of
      (True,  _) -> []
      (False, x) -> "packages:" : ["  " <> pathToText (unAbsFile x)]

  pure cabalFile

runProc
  :: HasCallStack
  => Handle
  -> Maybe OsPath
  -> String
  -> [String]
  -> IO ()
  -> ([String] -> Int -> IO (Doc ann))
  -> IO ()
runProc out cwd cmd args onSuccess msgOnError = do
  cwd' <- traverse OsPath.decodeUtf cwd
  let p = setStdin nullStream
        $ setStdout (useHandleClose out)
        $ setStderr (useHandleClose out)
        $ maybe id setWorkingDir cwd'
        $ proc cmd args

  when debug $
    putStrLn $ "Running " ++ show (cmd : args) ++ " in " ++ show cwd

  withProcessWait p $ \p' -> do
    exitCode <- waitExitCode p'
    hClose out
    case exitCode of
      ExitFailure x ->
        assertFailure . renderString =<< msgOnError (cmd : args) x
      ExitSuccess   ->
        onSuccess

_runProc'
  :: Maybe FilePath
  -> String
  -> [String]
  -> (Int -> Text -> Text -> Doc ann)
  -> IO ()
_runProc' cwd cmd args msgOnError = do
  runProcCaptureOutput cwd cmd args (\_ _ -> pure ()) msgOnError

runProcCaptureOutput
  :: HasCallStack
  => Maybe FilePath
  -> String
  -> [String]
  -> (Text -> Text -> IO a)
  -> (Int -> Text -> Text -> Doc ann)
  -> IO a
runProcCaptureOutput cwd cmd args processOutput msgOnError = do
  let p = setStdin nullStream
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
      ExitSuccess   -> processOutput stdOut stdErr

packagesSkipTest :: Set Text
packagesSkipTest = S.fromList
  [ "haskoin-node"
  , "hedis"
  , "skews"
  , "zeromq4-patterns"
  , "amqp" -- Tests are missing module in Hackage distribution and cannot be built
  , "cabal-install" -- Depends on 'Cabal-described' package which is not on Hackage
  ]

debug :: Bool
debug = False
