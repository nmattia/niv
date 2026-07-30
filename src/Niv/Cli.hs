{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Niv.Cli where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Char (isSpace)
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HMS
import Data.HashMap.Strict.Extended
import Data.Hashable (Hashable)
import Data.List (find)
import qualified Data.Text as T
import Data.Text.Extended
import Data.Version (showVersion)
import qualified Network.HTTP.Simple as HTTP
import Niv.Cmd
import Niv.Git.Cmd
import Niv.GitHub.Cmd
import Niv.Local.Cmd
import Niv.Logger hiding (Job)
import qualified Niv.Logger
import Niv.Sources
import Niv.Update
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Help.Pretty as Opts
-- I died a little
import Paths_niv (version)
import qualified System.Directory as Dir
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)
import UnliftIO
import UnliftIO.Concurrent

-- | An IO Monad with some configuration:
-- * FindSourcesJson: how to find sources.json (known path, discover, etc)
-- * [Cmd]: the update types
newtype NIO a = NIO {runNIO :: ReaderT (FindSourcesJson, [Cmd]) IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadReader (FindSourcesJson, [Cmd]))

type Job = Niv.Logger.Job NIO

instance MonadUnliftIO NIO where
  withRunInIO = wrappedWithRunInIO NIO runNIO

getFindSourcesJson :: NIO FindSourcesJson
getFindSourcesJson = fst <$> ask

getCmds :: NIO [Cmd]
getCmds = snd <$> ask

li :: (MonadIO io) => IO a -> io a
li = liftIO

cli :: [String] -> IO ()
cli args = do
  ((fsj, colors), nio) <-
    pure args >>= Opts.handleParseResult . execParserPure' Opts.defaultPrefs opts
  setColors colors
  runReaderT (runNIO nio) (fsj, [gitCmd, localCmd, githubCmd])
  where
    execParserPure' pprefs pinfo [] =
      Opts.Failure $
        Opts.parserFailure pprefs pinfo (Opts.ShowHelpText Nothing) mempty
    execParserPure' pprefs pinfo as = Opts.execParserPure pprefs pinfo as
    opts = Opts.info ((,) <$> ((,) <$> parseFindSourcesJson <*> parseColors) <*> (parseCommand <**> Opts.helper <**> versionflag)) $ mconcat desc
    desc =
      [ Opts.fullDesc,
        Opts.headerDoc $
          Just $
            Opts.vcat
              [ "niv - dependency manager for Nix projects",
                "",
                "version:"
                  Opts.<+> Opts.pretty (showVersion version)
              ]
      ]
    parseFindSourcesJson =
      AtPath
        <$> Opts.strOption
          ( Opts.long "sources-file"
              <> Opts.short 's'
              <> Opts.metavar "FILE"
              <> Opts.help "Use FILE instead of nix/sources.json"
          )
        <|> pure Auto
    parseColors =
      (\case True -> Never; False -> Always)
        <$> Opts.switch
          ( Opts.long "no-colors"
              <> Opts.help "Don't use colors in output"
          )
    versionflag :: Opts.Parser (a -> a)
    versionflag =
      Opts.abortOption (Opts.InfoMsg (showVersion version)) $
        mconcat
          [Opts.long "version", Opts.hidden, Opts.help "Print version"]

parseCommand :: Opts.Parser (NIO ())
parseCommand =
  Opts.subparser
    ( Opts.command "init" parseCmdInit
        <> Opts.command "add" parseCmdAdd
        <> Opts.command "show" parseCmdShow
        <> Opts.command "update" parseCmdUpdate
        <> Opts.command "modify" parseCmdModify
        <> Opts.command "drop" parseCmdDrop
        <> Opts.command "version" parseCmdVersion
    )
    <|> Opts.subparser (Opts.internal <> Opts.command "debug" parseCmdDebug)

parsePackageName :: Opts.Parser PackageName
parsePackageName =
  PackageName
    <$> Opts.argument Opts.str (Opts.metavar "PACKAGE")

parsePackage :: Opts.Parser (PackageName, PackageSpec)
parsePackage = (,) <$> parsePackageName <*> parsePackageSpec githubCmd

-------------------------------------------------------------------------------
-- INIT
-------------------------------------------------------------------------------

-- | Whether or not to fetch nixpkgs
data FetchNixpkgs
  = NoNixpkgs
  | NixpkgsFast -- Pull latest known nixpkgs
  | NixpkgsCustom T.Text Nixpkgs -- branch, nixpkgs
  deriving (Show)

data Nixpkgs = Nixpkgs T.Text T.Text -- owner, repo

instance Show Nixpkgs where
  show (Nixpkgs o r) = T.unpack o <> "/" <> T.unpack r

parseCmdInit :: Opts.ParserInfo (NIO ())
parseCmdInit = Opts.info (cmdInit <$> parseNixpkgs <**> Opts.helper) $ mconcat desc
  where
    desc =
      [ Opts.fullDesc,
        Opts.progDesc
          "Initialize a Nix project. Existing files won't be modified."
      ]

parseNixpkgs :: Opts.Parser FetchNixpkgs
parseNixpkgs = parseNixpkgsFast <|> parseNixpkgsLatest <|> parseNixpkgsCustom <|> parseNoNixpkgs <|> pure NixpkgsFast
  where
    parseNixpkgsFast =
      Opts.flag'
        NixpkgsFast
        ( Opts.long "fast"
            <> Opts.help "Use the latest nixpkgs cached at 'https://github.com/nmattia/niv/blob/master/data/nixpkgs.json'. This is the default."
        )
    parseNixpkgsLatest =
      Opts.flag'
        (NixpkgsCustom "master" (Nixpkgs "NixOS" "nixpkgs"))
        ( Opts.long "latest"
            <> Opts.help "Pull the latest unstable nixpkgs from NixOS/nixpkgs."
        )
    parseNixpkgsCustom =
      flip NixpkgsCustom
        <$> Opts.option
          customNixpkgsReader
          ( Opts.long "nixpkgs"
              <> Opts.showDefault
              <> Opts.help "Use a custom nixpkgs repository from GitHub."
              <> Opts.metavar "OWNER/REPO"
          )
        <*> Opts.strOption
          ( Opts.long "nixpkgs-branch"
              <> Opts.short 'b'
              <> Opts.help "The nixpkgs branch when using --nixpkgs ...."
              <> Opts.showDefault
          )
    parseNoNixpkgs =
      Opts.flag'
        NoNixpkgs
        ( Opts.long "no-nixpkgs"
            <> Opts.help "Don't add a nixpkgs entry to sources.json."
        )
    customNixpkgsReader = Opts.maybeReader $ \(T.pack -> repo) -> case T.splitOn "/" repo of
      [owner, reponame] -> Just (Nixpkgs owner reponame)
      _ -> Nothing

cmdInit :: FetchNixpkgs -> NIO ()
cmdInit nixpkgs = do
  fsj <- getFindSourcesJson

  -- Writes all the default files

  void $ job' "sources.nix (file)" $ do
    let path = pathNixSourcesNix
    exists <- liftIO $ Dir.doesFileExist path
    if exists
      then do
        content <- liftIO $ B.readFile path
        when (shouldUpdateNixSourcesNix content) $ do
          tsay' "updating sources.nix"
          liftIO $ B.writeFile path initNixSourcesNixContent
      else
        createFile path initNixSourcesNixContent
    case fsj of
      Auto -> pure ()
      AtPath fp -> noteUpdateSourcesNixForPath fp

  -- returns whether we should initialize nixpkgs
  sourcesJsonResult <- job' "sources.json (file)" $ do
    let path = pathNixSourcesJson fsj
    exists <- liftIO $ Dir.doesFileExist path
    if exists
      then do
        tsay' $ T.pack path <> " already exists"
        pure False
      else do
        createFile path initNixSourcesJsonContent
        pure True

  case sourcesJsonResult of
    Left () -> liftIO exitFailure
    Right False -> pure () -- not initializing nixpkgs
    Right True -> modifySources $ \sources -> do
      result <- job' "nixpkgs" (initNixpkgs sources)
      case result of
        Right sources' -> pure sources'
        Left () -> liftIO exitFailure
  where
    initNixpkgs sources = case nixpkgs of
      NoNixpkgs -> say' "Not importing 'nixpkgs'." >> pure sources
      NixpkgsFast -> do
        say' "Using known 'nixpkgs' ..."
        packageSpec <- HTTP.getResponseBody <$> HTTP.httpJSON "https://raw.githubusercontent.com/nmattia/niv/master/data/nixpkgs.json"
        applyAdd sources (PackageName "nixpkgs", packageSpec)
      NixpkgsCustom branch (Nixpkgs owner repo) -> do
        say' "Importing 'nixpkgs' ..."
        applyAdd
          sources
          ( PackageName "nixpkgs",
            PackageSpec $
              KM.fromList
                [ "owner" .= owner,
                  "repo" .= repo,
                  "branch" .= branch
                ]
          )

createFile :: FilePath -> B.ByteString -> Job ()
createFile path content = do
  let dir = takeDirectory path
  liftIO $ Dir.createDirectoryIfMissing True dir
  say' $ "Creating " <> path
  liftIO $ B.writeFile path content

-------------------------------------------------------------------------------
-- ADD
-------------------------------------------------------------------------------

parseCmdAdd :: Opts.ParserInfo (NIO ())
parseCmdAdd =
  Opts.info
    ((parseCommands <|> parseShortcuts) <**> Opts.helper)
    $ description githubCmd
  where
    -- XXX: this should parse many shortcuts (github, git). Right now we only
    -- parse GitHub because the git interface is still experimental.  note to
    -- implementer: it'll be tricky to have the correct arguments show up
    -- without repeating "PACKAGE PACKAGE PACKAGE" for every package type.
    parseShortcuts = parseShortcut githubCmd
    parseShortcut cmd = uncurry cmdAdd <$> parseShortcutArgs cmd
    parseCmd cmd = uncurry cmdAdd <$> parseCmdArgs cmd
    parseCmdAddGit =
      Opts.info (parseCmd gitCmd <**> Opts.helper) (description gitCmd)
    parseCmdAddLocal =
      Opts.info (parseCmd localCmd <**> Opts.helper) (description localCmd)
    parseCmdAddGitHub =
      Opts.info (parseCmd githubCmd <**> Opts.helper) (description githubCmd)
    parseCommands =
      Opts.subparser
        ( Opts.hidden
            <> Opts.commandGroup "Experimental commands:"
            <> Opts.command "git" parseCmdAddGit
            <> Opts.command "github" parseCmdAddGitHub
            <> Opts.command "local" parseCmdAddLocal
        )

-- | only used in shortcuts (niv add foo/bar ...) because PACKAGE is NOT
-- optional
parseShortcutArgs :: Cmd -> Opts.Parser (PackageName, Attrs)
parseShortcutArgs cmd = collapse <$> parseNameAndShortcut <*> parsePackageSpec cmd
  where
    collapse specAndName pspec = (pname, specToLockedAttrs $ pspec <> baseSpec)
      where
        (pname, baseSpec) = case specAndName of
          ((_, spec), Just pname') -> (pname', PackageSpec spec)
          ((pname', spec), Nothing) -> (pname', PackageSpec spec)
    parseNameAndShortcut =
      (,)
        <$> Opts.argument
          (Opts.maybeReader (parseCmdShortcut cmd . T.pack))
          (Opts.metavar "PACKAGE")
        <*> optName
    optName =
      Opts.optional $
        PackageName
          <$> Opts.strOption
            ( Opts.long "name"
                <> Opts.short 'n'
                <> Opts.metavar "NAME"
                <> Opts.help "Set the package name to <NAME>"
            )

-- | only used in command (niv add <cmd> ...) because PACKAGE is optional
parseCmdArgs :: Cmd -> Opts.Parser (PackageName, Attrs)
parseCmdArgs cmd = collapse <$> parseNameAndShortcut <*> parsePackageSpec cmd
  where
    collapse specAndName pspec = (pname, specToLockedAttrs $ pspec <> baseSpec)
      where
        (pname, baseSpec) = case specAndName of
          (Just (_, spec), Just pname') -> (pname', PackageSpec spec)
          (Just (pname', spec), Nothing) -> (pname', PackageSpec spec)
          (Nothing, Just pname') -> (pname', PackageSpec KM.empty)
          (Nothing, Nothing) -> (PackageName "unnamed", PackageSpec KM.empty)
    parseNameAndShortcut =
      (,)
        <$> Opts.optional
          ( Opts.argument
              (Opts.maybeReader (parseCmdShortcut cmd . T.pack))
              (Opts.metavar "PACKAGE")
          )
        <*> optName
    optName =
      Opts.optional $
        PackageName
          <$> Opts.strOption
            ( Opts.long "name"
                <> Opts.short 'n'
                <> Opts.metavar "NAME"
                <> Opts.help "Set the package name to <NAME>"
            )

cmdAdd :: PackageName -> Attrs -> NIO ()
cmdAdd packageName attrs = do
  let spec = attrsToSpec attrs
  modifySources $ \sources -> do
    result <- job' (unPackageName packageName) $ do
      tsay' "adding package..."
      result <- applyAdd sources (packageName, spec)
      tsay' "package added"
      pure result
    case result of
      Left () -> liftIO exitFailure
      Right sources' -> pure sources'

applyAdd :: Sources -> (PackageName, PackageSpec) -> Job Sources
applyAdd (unSources -> sources) (packageName, defaultSpec) = do
  cmds <- lift getCmds

  -- infer what command (git, github, etc) to use to add the package
  cmd <- case inferCmd cmds defaultSpec of
    Just cmd -> pure cmd
    Nothing -> abortNoSuitableCommandForAdd packageName

  when (HMS.member packageName sources) $
    abortCannotAddPackageExists packageName

  let attrs = specToLockedAttrs defaultSpec
  finalSpec <- attrsToSpec <$> doUpdate attrs cmd

  pure $ Sources $ HMS.insert packageName finalSpec sources

-------------------------------------------------------------------------------
-- SHOW
-------------------------------------------------------------------------------

parseCmdShow :: Opts.ParserInfo (NIO ())
parseCmdShow =
  Opts.info
    ((cmdShow <$> Opts.optional parsePackageName) <**> Opts.helper)
    Opts.fullDesc

-- TODO: nicer output
cmdShow :: Maybe PackageName -> NIO ()
cmdShow = \case
  Just packageName -> do
    fsj <- getFindSourcesJson
    sources <- unSources <$> li (getSources fsj)
    case HMS.lookup packageName sources of
      Just pspec -> showPackage packageName pspec
      Nothing -> li $ abortCannotShowNoSuchPackage packageName
  Nothing -> do
    fsj <- getFindSourcesJson
    sources <- unSources <$> li (getSources fsj)
    forWithKeyM_ sources showPackage

showPackage :: (MonadIO io) => PackageName -> PackageSpec -> io ()
showPackage (PackageName pname) (PackageSpec spec) = do
  tsay $ tbold pname
  forM_ (KM.toList spec) $ \(attrName, attrValValue) -> do
    let attrValue = case attrValValue of
          Aeson.String str -> str
          _ -> tfaint "<barabajagal>"
    tsay $ "  " <> K.toText attrName <> ": " <> attrValue

-------------------------------------------------------------------------------
-- UPDATE
-------------------------------------------------------------------------------

parseCmdUpdate :: Opts.ParserInfo (NIO ())
parseCmdUpdate =
  Opts.info
    ((cmdUpdate <$> Opts.optional parsePackage) <**> Opts.helper)
    $ mconcat desc
  where
    desc =
      [ Opts.fullDesc,
        Opts.progDesc "Update dependencies",
        Opts.headerDoc $
          Just $
            Opts.nest 2 $
              Opts.vcat
                [ "Examples:",
                  "",
                  Opts.fill 30 "niv update" Opts.<+> "# update all packages",
                  Opts.fill 30 "niv update nixpkgs" Opts.<+> "# update nixpkgs",
                  Opts.fill 30 "niv update my-package -v beta-0.2" Opts.<+> "# update my-package to version \"beta-0.2\""
                ]
      ]

specToFreeAttrs :: PackageSpec -> Attrs
specToFreeAttrs = KM.toHashMapText . fmap (Free,) . unPackageSpec

specToLockedAttrs :: PackageSpec -> Attrs
specToLockedAttrs = KM.toHashMapText . fmap (Locked,) . unPackageSpec

-- | find a matching Cmd for the PackageSpec
inferCmd :: [Cmd] -> PackageSpec -> Maybe Cmd
inferCmd cmds spec = do
  find (\cmd -> acceptsCmd cmd spec) cmds

updatePackage :: PackageSpec -> Maybe PackageSpec -> Job PackageSpec
updatePackage defaultSpec mSpec = do
  let defAttrs = specToFreeAttrs defaultSpec
      attrs = maybe defAttrs (\cliSpec -> specToLockedAttrs cliSpec <> defAttrs) mSpec

  cmds <- lift getCmds

  -- infer what command (git, github, etc) to use to update the package
  cmd <- case inferCmd cmds defaultSpec of
    Just cmd -> pure cmd
    Nothing -> abortNoSuitableCommandForUpdate'

  tsay' "updating..."
  result <- attrsToSpec <$> doUpdate attrs cmd
  tsay' "package updated"
  pure result

-- | Update many packages.
-- For each package, the package name, attrs-to-update as well as original state are given.
-- For each package, the package name and final state are returned.
updatePackages :: Sources -> [(PackageName, Maybe PackageSpec)] -> NIO [(PackageName, Either () PackageSpec)]
updatePackages sources packageUpdates = do
  let maxNameLength = maximum $ (\(p, _) -> T.length $ unPackageName p) <$> packageUpdates
      padName (PackageName p) = p <> T.replicate (maxNameLength - T.length p) " "

  forM packageUpdates $ \(packageName, mCliSpec) -> do
    result <- job' (padName packageName) $ do
      case HMS.lookup packageName (unSources sources) of
        Just spec -> updatePackage spec mCliSpec
        Nothing -> abortCannotUpdateNoSuchPackage packageName
    case result of
      Right spec -> pure (packageName, Right spec)
      Left _ -> pure (packageName, Left ())

applyUpdate :: Sources -> Maybe (PackageName, PackageSpec) -> NIO Sources
applyUpdate sources updateType = do
  -- prepare the updates
  let packageUpdates = case updateType of
        -- no package specified => update everything
        Nothing -> HMS.toList (unSources sources) <&> (\(k, _) -> (k, Nothing))
        -- one package with new attrs specified => update just that
        Just (packageName, cliSpec) -> [(packageName, Just cliSpec)]

  -- update all packages and separate failures from successes
  (errs, updatedPackages) <- partitionUpdateFailures <$> updatePackages sources packageUpdates

  -- if there are any errors, abort the update before we serialize the new results.
  -- (not by necessity, just because this is legacy behavior)
  -- TODO: this is counter intuitive
  unless (null errs) $ liftIO exitFailure

  -- return the updated sources
  pure $
    Sources $
      foldl' (\acc (packageName, newSpec) -> HMS.insert packageName newSpec acc) (unSources sources) updatedPackages

cmdUpdate :: Maybe (PackageName, PackageSpec) -> NIO ()
cmdUpdate mPackageNameAndSpec =
  modifySources $ \sources -> applyUpdate sources mPackageNameAndSpec

-- | pretty much tryEvalUpdate but we might issue some warnings first
doUpdate :: Attrs -> Cmd -> Job Attrs
doUpdate attrs cmd = do
  forM_ (extraLogs cmd attrs) tsay'
  result <- liftIO $ tryEvalUpdate attrs (updateCmd cmd)
  case result of
    Right attrs' -> pure attrs'
    Left e -> abort' $ T.show e

partitionUpdateFailures :: [(PackageName, Either l r)] -> ([(PackageName, l)], [(PackageName, r)])
partitionUpdateFailures =
  foldl'
    ( \(lefts, rights) (packageName, res) -> case res of
        Left left -> ((packageName, left) : lefts, rights)
        Right right -> (lefts, (packageName, right) : rights)
    )
    ([], [])

partitionEithersHMS ::
  (Eq k, Hashable k) =>
  HMS.HashMap k (Either a b) ->
  (HMS.HashMap k a, HMS.HashMap k b)
partitionEithersHMS =
  flip HMS.foldlWithKey' (HMS.empty, HMS.empty) $ \(ls, rs) k -> \case
    Left l -> (HMS.insert k l ls, rs)
    Right r -> (ls, HMS.insert k r rs)

-------------------------------------------------------------------------------
-- MODIFY
-------------------------------------------------------------------------------

parseCmdModify :: Opts.ParserInfo (NIO ())
parseCmdModify =
  Opts.info
    ((cmdModify <$> parsePackageName <*> optName <*> parsePackageSpec githubCmd) <**> Opts.helper)
    $ mconcat desc
  where
    desc =
      [ Opts.fullDesc,
        Opts.progDesc "Modify dependency attributes without performing an update",
        Opts.headerDoc $
          Just $
            Opts.vcat
              [ "Examples:",
                "",
                "  niv modify nixpkgs -v beta-0.2",
                "  niv modify nixpkgs -a branch=nixpkgs-unstable"
              ]
      ]
    optName =
      Opts.optional $
        PackageName
          <$> Opts.strOption
            ( Opts.long "name"
                <> Opts.short 'n'
                <> Opts.metavar "NAME"
                <> Opts.help "Set the package name to <NAME>"
            )

cmdModify :: PackageName -> Maybe PackageName -> PackageSpec -> NIO ()
cmdModify packageName mNewName cliSpec = do
  modifySources $ \sources -> do
    result <- job' (unPackageName packageName) $ do
      tsay' "modifying package..."
      result <- applyModify sources packageName mNewName cliSpec
      tsay' "package modified"
      pure result

    case result of
      Left () -> liftIO exitFailure
      Right sources' -> pure sources'

applyModify :: Sources -> PackageName -> Maybe PackageName -> PackageSpec -> Job Sources
applyModify (unSources -> sources) packageName mNewName cliSpec = do
  finalSpec <- case HMS.lookup packageName sources of
    Just defaultSpec -> pure $ attrsToSpec (specToLockedAttrs cliSpec <> specToFreeAttrs defaultSpec)
    Nothing -> abortCannotModifyNoSuchPackage packageName
  case mNewName of
    Just newName -> do
      when (HMS.member newName sources) $
        abortCannotRenamePackageExists packageName newName
      pure $ Sources $ HMS.insert newName finalSpec $ HMS.delete packageName sources
    Nothing ->
      pure $ Sources $ HMS.insert packageName finalSpec sources

-------------------------------------------------------------------------------
-- DROP
-------------------------------------------------------------------------------

parseCmdDrop :: Opts.ParserInfo (NIO ())
parseCmdDrop =
  Opts.info
    ( (cmdDrop <$> parsePackageName <*> parseDropAttributes)
        <**> Opts.helper
    )
    $ mconcat desc
  where
    desc =
      [ Opts.fullDesc,
        Opts.progDesc "Drop dependency",
        Opts.headerDoc $
          Just $
            Opts.vcat
              [ "Examples:",
                "",
                "  niv drop jq",
                "  niv drop my-package version"
              ]
      ]
    parseDropAttributes :: Opts.Parser [T.Text]
    parseDropAttributes =
      many $
        Opts.argument Opts.str (Opts.metavar "ATTRIBUTE")

cmdDrop :: PackageName -> [T.Text] -> NIO ()
cmdDrop packageName = \case
  [] -> do
    modifySources $ \sources -> do
      result <- job' (unPackageName packageName) $ do
        tsay' "dropping package..."
        result <- applyPackageDrop sources packageName
        tsay' "package dropped"
        pure result
      case result of
        Left () -> liftIO exitFailure
        Right sources' -> pure sources'
  attrs -> do
    tsay $ "Dropping attributes: " <> T.intercalate " " attrs
    tsay $ "In package: " <> unPackageName packageName
    modifySources $ \sources -> do
      result <- job' (unPackageName packageName) $ do
        tsay' "dropping package attributes..."
        result <- applyPackageAttributesDrop sources packageName attrs
        tsay' "package attributes dropped"
        pure result
      case result of
        Left () -> liftIO exitFailure
        Right sources' -> pure sources'

applyPackageDrop :: Sources -> PackageName -> Job Sources
applyPackageDrop (unSources -> sources) packageName = do
  unless (HMS.member packageName sources) $
    abortCannotDropNoSuchPackage packageName
  pure $ Sources $ HMS.delete packageName sources

applyPackageAttributesDrop :: Sources -> PackageName -> [T.Text] -> Job Sources
applyPackageAttributesDrop (unSources -> sources) packageName attrs = do
  packageSpec <- case HMS.lookup packageName sources of
    Nothing -> abortCannotAttributesDropNoSuchPackage packageName
    Just (PackageSpec packageSpec) ->
      pure $
        PackageSpec $
          KM.mapMaybeWithKey
            (\k v -> if K.toText k `elem` attrs then Nothing else Just v)
            packageSpec
  pure $ Sources $ HMS.insert packageName packageSpec sources

-------------------------------------------------------------------------------
-- VERSION
-------------------------------------------------------------------------------

parseCmdVersion :: Opts.ParserInfo (NIO ())
parseCmdVersion =
  Opts.info
    ( pure (tsay $ T.pack $ showVersion version)
        <**> Opts.helper
    )
    $ mconcat desc
  where
    desc =
      [ Opts.fullDesc,
        Opts.progDesc "Print version"
      ]

-------------------------------------------------------------------------------
-- DEBUG: some debugging helpers (internal)
-------------------------------------------------------------------------------

-- | Collection of help, debug and test output for bug reports & tests
parseCmdDebug :: Opts.ParserInfo (NIO ())
parseCmdDebug =
  Opts.info
    ( Opts.subparser
        ( Opts.command "job-hello-world" (Opts.info (pure $ li jobHelloWorld) mempty)
            <> Opts.command "job-note" (Opts.info (pure $ li jobNote) mempty)
            <> Opts.command "job-note-multiline" (Opts.info (pure $ li jobNoteMultiline) mempty)
            <> Opts.command "job-warn" (Opts.info (pure $ li jobWarn) mempty)
            <> Opts.command "job-err" (Opts.info (pure $ li jobErr) mempty)
        )
    )
    mempty

-- "hello world" inside a job.
jobHelloWorld :: IO ()
jobHelloWorld = void $ job' "test" $ do
  threadDelay 600000
  say' "hello"
  threadDelay 600000
  say' "world"
  threadDelay 600000

-- TODO
jobNote :: IO ()
jobNote = void $ job' "test-note" $ do
  threadDelay 600000
  note' "hello"
  threadDelay 600000

jobNoteMultiline :: IO ()
jobNoteMultiline = void $ job' "test-note-multiline" $ do
  threadDelay 600000
  noteUpdateSourcesNixForPath "foobar.txt"
  threadDelay 600000
  note' "Oh yeah\nhello world"
  threadDelay 600000

jobEveryAdmonition :: IO ()
jobEveryAdmonition = void $ job' "every-admonition" $ do
  warn' "some warning"
  note' "some note"
  abort' "some error"

-- TODO
jobWarn :: IO ()
jobWarn = void $ job' "test-warn" $ do
  threadDelay 600000
  warn' "hello"
  threadDelay 600000

-- TODO
jobErr :: IO ()
jobErr = void $ job' "test-err" $ do
  threadDelay 600000
  _ <- abort' "nope, not working"
  threadDelay 600000

-- TODO
jobMulti :: IO ()
jobMulti = do
  void $ job' "a" $ tsay' "message"
  void $ job' "ab" $ tsay' "message"
  void $ job' "abc-def" $ tsay' "message"
  void $ job' "hello" $ tsay' "message"
  void $ job' "world" $ tsay' "message"
  void $ job' "nothing" $ tsay' "message"

-------------------------------------------------------------------------------
-- Files and their content
-------------------------------------------------------------------------------

-- helper for modifying the sources file
modifySources :: (Sources -> NIO Sources) -> NIO ()
modifySources upd = do
  fsj <- getFindSourcesJson
  sources <- liftIO $ getSources fsj
  sources' <- upd sources
  liftIO $ setSources fsj sources'

-- | Checks if content is different than default and if it does /not/ contain
-- a comment line with @niv: no_update@
shouldUpdateNixSourcesNix :: B.ByteString -> Bool
shouldUpdateNixSourcesNix content =
  content /= initNixSourcesNixContent
    && not (any lineForbids (B8.lines content))
  where
    lineForbids :: B8.ByteString -> Bool
    lineForbids str =
      case B8.uncons (B8.dropWhile isSpace str) of
        Just ('#', rest) -> case B8.stripPrefix "niv:" (B8.dropWhile isSpace rest) of
          Just rest' -> case B8.stripPrefix "no_update" (B8.dropWhile isSpace rest') of
            Just {} -> True
            _ -> False
          _ -> False
        _ -> False

---
-- note
--

noteUpdateSourcesNixForPath :: (MonadIO io) => FilePath -> Niv.Logger.Job io ()
noteUpdateSourcesNixForPath fp = do
  note' $
    T.unlines
      [ "You are using a custom path for sources.json.",
        "You need to configure the sources.nix to use " <> tbold (T.pack fp) <> ":",
        "",
        tbold "      import sources.nix { sourcesFile = PATH ; }; ",
        "",
        T.unwords
          [ "  where",
            tbold "PATH",
            "is the relative path from sources.nix to",
            tbold (T.pack fp) <> "."
          ]
      ]

-------------------------------------------------------------------------------
-- Abort
-------------------------------------------------------------------------------

abortCannotAddPackageExists :: PackageName -> Job a
abortCannotAddPackageExists (PackageName n) =
  abort' $
    T.unlines
      [ "Cannot add package " <> n <> ".",
        "The package already exists. Use",
        "  niv drop " <> n,
        "and then re-add the package. Alternatively use",
        "  niv update " <> n <> " --attribute foo=bar",
        "to update the package's attributes."
      ]

abortCannotRenamePackageExists :: PackageName -> PackageName -> Job a
abortCannotRenamePackageExists (PackageName from) (PackageName to) =
  abort' $
    T.unlines
      [ "Cannot rename package " <> from <> " to " <> to <> ".",
        "Package " <> to <> " already exists."
      ]

abortCannotUpdateNoSuchPackage :: PackageName -> Job a
abortCannotUpdateNoSuchPackage (PackageName n) =
  abort' $
    T.unlines
      [ "Cannot update package " <> n <> ".",
        "The package doesn't exist. Use",
        "  niv add " <> n,
        "to add the package."
      ]

abortCannotModifyNoSuchPackage :: PackageName -> Job a
abortCannotModifyNoSuchPackage (PackageName n) =
  abort' $
    T.unlines
      [ "Cannot modify package " <> n <> ".",
        "The package doesn't exist. Use",
        "  niv add " <> n,
        "to add the package."
      ]

abortCannotDropNoSuchPackage :: PackageName -> Job a
abortCannotDropNoSuchPackage (PackageName n) =
  abort' $
    T.unlines
      [ "Cannot drop package " <> n <> ".",
        "The package doesn't exist."
      ]

abortCannotAttributesDropNoSuchPackage :: PackageName -> Job a
abortCannotAttributesDropNoSuchPackage (PackageName n) =
  abort' $
    T.unlines
      [ "Cannot drop attributes of package " <> n <> ".",
        "The package doesn't exist."
      ]

abortNoSuitableCommandForUpdate' :: Job a
abortNoSuitableCommandForUpdate' =
  abort' "don't know how to update package"

abortNoSuitableCommandForUpdate :: PackageName -> Job a
abortNoSuitableCommandForUpdate pname =
  abort' $ "Don't know how to update package: " <> unPackageName pname

abortNoSuitableCommandForAdd :: PackageName -> Job a
abortNoSuitableCommandForAdd pname =
  abort' $ "Don't know how to add package: " <> unPackageName pname

-- Some legacy abort commands

abortCannotShowNoSuchPackage :: PackageName -> IO a
abortCannotShowNoSuchPackage (PackageName n) =
  abort $
    T.unlines
      [ "Cannot show package " <> n <> ".",
        "The package doesn't exist."
      ]
