{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Data.Either (partitionEithers)
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HMS
import Data.HashMap.Strict.Extended
import Data.List (find)
import Data.String.QQ (s)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Version (showVersion)
import qualified Network.HTTP.Simple as HTTP
import Niv.Cmd
import Niv.Git.Cmd hiding (abort)
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

cli :: [String] -> IO ()
cli args = do
  ((fsj, colors), nio) <-
    pure args >>= Opts.handleParseResult . execParserPure' Opts.defaultPrefs opts
  setColors colors
  runReaderT (runNIO nio) (fsj, [gitCmd, localCmd, githubCmd])
  warnIfOutdated
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

  void $ job "sources.nix (file)" $ do
    let path = pathNixSourcesNix
    exists <- liftIO $ Dir.doesFileExist path
    if exists
      then do
        content <- liftIO $ B.readFile path
        when (shouldUpdateNixSourcesNix content) $ do
          say "updating sources.nix"
          liftIO $ B.writeFile path initNixSourcesNixContent
      else
        createFile path initNixSourcesNixContent
    case fsj of
      Auto -> pure ()
      AtPath fp -> noteUpdateSourcesNixForPath fp

  -- returns whether we should initialize nixpkgs
  sourcesJsonResult <- job "sources.json (file)" $ do
    let path = pathNixSourcesJson fsj
    exists <- liftIO $ Dir.doesFileExist path
    if exists
      then do
        say $ T.pack path <> " already exists"
        pure False
      else do
        createFile path initNixSourcesJsonContent
        pure True

  case (sourcesJsonResult, nixpkgs) of
    (Left (), _) -> liftIO exitFailure
    (Right False, _) -> pure () -- not initializing nixpkgs
    (_, NoNixpkgs) -> pure () -- not initializing nixpkgs
    (Right True, NixpkgsFast) -> do
      result <- job "nixpkgs" $ do
        say "Using known 'nixpkgs' ..."
        spec <- HTTP.getResponseBody <$> HTTP.httpJSON "https://raw.githubusercontent.com/nmattia/niv/master/data/nixpkgs.json"
        updatePackage (specToLockedAttrs spec)

      case result of
        Right spec' -> writeSourcesEntry (PackageName "nixpkgs") spec'
        Left () -> liftIO exitFailure
    (Right True, NixpkgsCustom branch (Nixpkgs owner repo)) -> do
      result <- job "nixpkgs" $ do
        say "Importing 'nixpkgs' ..."
        updatePackage $
          specToLockedAttrs $
            PackageSpec $
              KM.fromList
                [ "owner" .= owner,
                  "repo" .= repo,
                  "branch" .= branch
                ]

      case result of
        Right spec' -> writeSourcesEntry (PackageName "nixpkgs") spec'
        Left () -> liftIO exitFailure

createFile :: FilePath -> B.ByteString -> Job ()
createFile path content = do
  let dir = takeDirectory path
  liftIO $ Dir.createDirectoryIfMissing True dir
  say $ "Creating " <> T.pack path
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
  sources <- unSources <$> readSources
  when (HMS.member packageName sources) $ abortCannotAddPackageExists packageName

  result <- job (unPackageName packageName) $ do
    say "updating new package..."
    result <- updatePackage (specToLockedAttrs spec)
    say "package updated"
    pure result

  case result of
    Right spec' -> writeSourcesEntry packageName spec'
    Left () -> liftIO exitFailure

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
    sources <- unSources <$> readSources
    case HMS.lookup packageName sources of
      Just pspec -> showPackage packageName pspec
      Nothing -> abortNoSuchPackage packageName
  Nothing -> do
    sources <- unSources <$> readSources
    forWithKeyM_ sources showPackage

showPackage :: (MonadIO io) => PackageName -> PackageSpec -> io ()
showPackage (PackageName pname) (PackageSpec spec) = do
  liftIO $ T.putStrLn $ tbold pname
  forM_ (KM.toList spec) $ \(attrName, attrValValue) -> do
    let attrValue = case attrValValue of
          Aeson.String str -> str
          _ -> tfaint "<barabajagal>"
    liftIO $ T.putStrLn $ "  " <> K.toText attrName <> ": " <> attrValue

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

-- update the attributes and return the updated spec
updatePackage :: Attrs -> Job PackageSpec
updatePackage attrs = do
  cmds <- lift getCmds

  -- infer what command (git, github, etc) to use to update the package
  cmd <- case inferCmd cmds (attrsToSpec attrs) of
    Just cmd -> pure cmd
    Nothing -> abortNoSuitableCommand

  say "updating..."
  result <- attrsToSpec <$> doUpdate attrs cmd
  say "package updated"
  pure result

-- | Update many packages.
-- For each package, the package name, attrs-to-update as well as original state are given.
-- For each package, the package name and final state are returned.
updatePackages :: [(PackageName, PackageSpec, Maybe PackageSpec)] -> NIO [Either () ()]
updatePackages packageUpdates = do

  -- prepare some padding for package names so that the output is aligned
  let maxNameLength = maximum $ (\(p, _, _) -> T.length $ unPackageName p) <$> packageUpdates
      padName (PackageName p) = p <> T.replicate (maxNameLength - T.length p) " "

  -- update all the packages, writing the new spec after each (successful) update and
  -- finally returning a list of all successes & failures statuses
  forM packageUpdates $ \(packageName, spec, mCliSpec) -> do
    let defAttrs = specToFreeAttrs spec
        attrs = maybe defAttrs (\cliSpec -> specToLockedAttrs cliSpec <> defAttrs) mCliSpec
    result <- job (padName packageName) $ updatePackage attrs
    case result of
      Right spec' -> do
        writeSourcesEntry packageName spec'
        pure $ Right ()
      Left _ -> pure $ Left ()

cmdUpdate :: Maybe (PackageName, PackageSpec) -> NIO ()
cmdUpdate updateType = do
  -- prepare the updates
  packageUpdates <- case updateType of
    -- no package specified => update everything
    Nothing -> do
      sources <- readSources
      pure $ HMS.toList (unSources sources) <&> (\(k, v) -> (k, v, Nothing))
    -- one package with new attrs specified => update just that
    Just (packageName, cliSpec) -> do
      sources <- readSources
      spec <- case HMS.lookup packageName (unSources sources) of
        Nothing -> abortNoSuchPackage packageName
        Just spec -> pure spec
      pure [(packageName, spec, Just cliSpec)]

  -- update all packages and separate failures from successes
  (errs, successes) <- partitionEithers <$> updatePackages packageUpdates

  -- print a short summary
  liftIO $ T.putStrLn ""
  unless (null successes) $ do
    liftIO $ T.putStrLn $ T.pack (show (length successes)) <> " package(s) updated successfully"
  unless (null errs) $ do
    liftIO $ T.putStrLn ""
    liftIO $ T.putStrLn $ T.pack (show (length errs)) <> " package(s) failed to update"
    liftIO exitFailure

-- | pretty much tryEvalUpdate but we might issue some warnings first
doUpdate :: Attrs -> Cmd -> Job Attrs
doUpdate attrs cmd = do
  forM_ (extraLogs cmd attrs) say
  result <- liftIO $ tryEvalUpdate attrs (updateCmd cmd)
  case result of
    Right attrs' -> pure attrs'
    Left e -> throwError $ T.show e

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

-- if mNewName is not null, then rename the package (remove original and insert the new one)
cmdModify :: PackageName -> Maybe PackageName -> PackageSpec -> NIO ()
cmdModify packageName mNewName cliSpec =
  modifySources $ \(unSources -> sources) -> do
    spec <- case HMS.lookup packageName sources of
      Nothing -> abortNoSuchPackage packageName
      Just spec -> pure spec

    let spec' = attrsToSpec (specToLockedAttrs cliSpec <> specToFreeAttrs spec)
    case mNewName of
      Just newName -> do
        when (HMS.member newName sources) $
          abortCannotRenamePackageExists packageName newName
        pure $ Sources $ HMS.insert newName spec' $ HMS.delete packageName sources
      Nothing ->
        pure $ Sources $ HMS.insert packageName spec' sources

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
  [] -> modifySources $ \(unSources -> sources) -> do
    unless (HMS.member packageName sources) $
      abortNoSuchPackage packageName
    pure $ Sources $ HMS.delete packageName sources
  attrs -> do
    liftIO $ T.putStrLn $ "Dropping attributes: " <> T.intercalate " " attrs
    liftIO $ T.putStrLn $ "In package: " <> unPackageName packageName
    sources <- unSources <$> readSources
    spec <- case HMS.lookup packageName sources of
      Nothing -> abortNoSuchPackage packageName
      Just spec -> pure spec
    let spec' = PackageSpec $ KM.mapMaybeWithKey (\k v -> if K.toText k `elem` attrs then Nothing else Just v) (unPackageSpec spec)
    writeSourcesEntry packageName spec'

-------------------------------------------------------------------------------
-- VERSION
-------------------------------------------------------------------------------

parseCmdVersion :: Opts.ParserInfo (NIO ())
parseCmdVersion =
  Opts.info
    ( pure (liftIO $ T.putStrLn $ T.pack $ showVersion version)
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
        ( Opts.command "job-hello-world" (Opts.info (pure $ liftIO jobHelloWorld) mempty)
            <> Opts.command "job-note" (Opts.info (pure $ liftIO jobNote) mempty)
            <> Opts.command "job-note-multiline" (Opts.info (pure $ liftIO jobNoteMultiline) mempty)
            <> Opts.command "job-every-admonition" (Opts.info (pure $ liftIO jobEveryAdmonition) mempty)
            <> Opts.command "job-multi" (Opts.info (pure $ liftIO jobMulti) mempty)
        )
    )
    mempty

-- "hello world" inside a job.
jobHelloWorld :: IO ()
jobHelloWorld = void $ job "test" $ do
  threadDelay 600000
  say "hello"
  threadDelay 600000
  say "world"
  threadDelay 600000

-- simple note
jobNote :: IO ()
jobNote = void $ job "test-note" $ do
  threadDelay 600000
  note "hello"
  threadDelay 600000

-- multiline notes
jobNoteMultiline :: IO ()
jobNoteMultiline = void $ job "test-note-multiline" $ do
  note $ "this is the first note\nwhich is a " <> tbold "multiline" <> " note"
  note "this is another note"

-- every admonition (note, warning, error)
jobEveryAdmonition :: IO ()
jobEveryAdmonition = void $ job "every-admonition" $ do
  warn "some warning"
  note "some note"
  throwError "some error"

-- multiple jobs
jobMulti :: IO ()
jobMulti = do
  void $ job "a" $ say "message"
  void $ job "ab" $ say "message"
  void $ job "abc-def" $ say "message"
  void $ job "hello" $ say "message"
  void $ job "world" $ say "message"
  void $ job "nothing" $ say "message"

-------------------------------------------------------------------------------
-- Files and their content
-------------------------------------------------------------------------------

-- Read the sources, throwing an IO error if there's an issue
getSources :: FindSourcesJson -> IO Sources
getSources fsj = do
  getSourcesEither fsj
    >>= either
      ( \case
          SourcesDoesntExist -> (abortSourcesDoesntExist fsj)
          SourceIsntJSON -> (abortSourcesIsntJSON fsj)
          SpecIsntAMap -> (abortSpecIsntAMap fsj)
      )
      pure

-- helper for modifying the sources file
modifySources :: (Sources -> NIO Sources) -> NIO ()
modifySources upd = do
  fsj <- getFindSourcesJson
  sources <- liftIO $ getSources fsj
  sources' <- upd sources
  liftIO $ setSources fsj sources'

-- Read the sources in NIO
readSources :: NIO Sources
readSources = do
  fsj <- getFindSourcesJson
  liftIO $ getSources fsj

-- Update or insert a spec
writeSourcesEntry :: PackageName -> PackageSpec -> NIO ()
writeSourcesEntry packageName spec = do
  fsj <- getFindSourcesJson
  sources <- liftIO $ getSources fsj
  let sources' = Sources $ HMS.insert packageName spec (unSources sources)
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

-------------------------------------------------------------------------------
-- MISC
-------------------------------------------------------------------------------

noteUpdateSourcesNixForPath :: (MonadIO io) => FilePath -> Niv.Logger.Job io ()
noteUpdateSourcesNixForPath fp = do
  note $
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

-- A job error if no update Cmd is suited to the package
abortNoSuitableCommand :: Job a
abortNoSuitableCommand =
  throwError "don't know how to update package"


-- proper aborts that exit niv (only used when there is no way to make
-- progress, like missing sources)

abort :: (MonadIO io) => T.Text -> io a
abort msg = do
  liftIO $ T.putStrLn $ T.unwords [tbold (tred "FATAL") <> ":", msg]
  liftIO exitFailure

abortNoSuchPackage :: (MonadIO io) => PackageName -> io a
abortNoSuchPackage (unPackageName -> packageName) =
  abort $ packageName <> ": no such package"

abortSourcesDoesntExist :: FindSourcesJson -> IO a
abortSourcesDoesntExist fsj = abort $ T.unlines [line1, line2]
  where
    line1 = "Cannot use " <> T.pack (pathNixSourcesJson fsj)
    line2 =
      [s|
The sources file does not exist! You may need to run 'niv init'.
|]

abortSourcesIsntJSON :: FindSourcesJson -> IO a
abortSourcesIsntJSON fsj = abort $ T.unlines [line1, line2]
  where
    line1 = "Cannot use " <> T.pack (pathNixSourcesJson fsj)
    line2 = "The sources file should be JSON."

abortSpecIsntAMap :: FindSourcesJson -> IO a
abortSpecIsntAMap fsj = abort $ T.unlines [line1, line2]
  where
    line1 = "Cannot use " <> T.pack (pathNixSourcesJson fsj)
    line2 =
      [s|
The package specifications in the sources file should be JSON maps from
attribute name to attribute value, e.g.:
  { "nixpkgs": { "foo": "bar" } }
|]

abortCannotRenamePackageExists :: PackageName -> PackageName -> NIO a
abortCannotRenamePackageExists (PackageName from) (PackageName to) =
  abort $
    T.unlines
      [ "Cannot rename package " <> from <> " to " <> to <> ".",
        "Package " <> to <> " already exists."
      ]

abortCannotAddPackageExists :: PackageName -> NIO a
abortCannotAddPackageExists (PackageName n) =
  abort $
    T.unlines
      [ "Cannot add package " <> n <> ".",
        "The package already exists. Use",
        "  niv drop " <> n,
        "and then re-add the package. Alternatively use",
        "  niv update " <> n <> " --attribute foo=bar",
        "to update the package's attributes."
      ]
