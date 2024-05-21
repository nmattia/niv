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
import qualified Data.HashMap.Strict as HMS
import Data.HashMap.Strict.Extended
import Data.Hashable (Hashable)
import qualified Data.Text as T
import Data.Text.Extended
import Data.Version (showVersion)
import qualified Network.HTTP.Simple as HTTP
import Niv.Cmd
import Niv.Git.Cmd
import Niv.GitHub.Cmd
import Niv.Local.Cmd
import Niv.Logger
import Niv.Sources
import Niv.Update
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Help.Pretty as Opts
-- I died a little
import Paths_niv (version)
import qualified System.Directory as Dir
import System.Environment (getArgs)
import System.FilePath (takeDirectory)
import UnliftIO

newtype NIO a = NIO {runNIO :: ReaderT FindSourcesJson IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader FindSourcesJson)

instance MonadUnliftIO NIO where
  withRunInIO = wrappedWithRunInIO NIO runNIO

getFindSourcesJson :: NIO FindSourcesJson
getFindSourcesJson = ask

li :: (MonadIO io) => IO a -> io a
li = liftIO

cli :: IO ()
cli = do
  ((fsj, colors), nio) <-
    getArgs >>= Opts.handleParseResult . execParserPure' Opts.defaultPrefs opts
  setColors colors
  runReaderT (runNIO nio) fsj
  where
    execParserPure' pprefs pinfo [] =
      Opts.Failure $
        Opts.parserFailure pprefs pinfo (Opts.ShowHelpText Nothing) mempty
    execParserPure' pprefs pinfo args = Opts.execParserPure pprefs pinfo args
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
    )

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
  job "Initializing" $ do
    fsj <- getFindSourcesJson
    -- Writes all the default files
    -- a path, a "create" function and an update function for each file.
    forM_
      [ ( pathNixSourcesNix,
          (`createFile` initNixSourcesNixContent),
          \path content -> do
            if shouldUpdateNixSourcesNix content
              then do
                say "Updating sources.nix"
                li $ B.writeFile path initNixSourcesNixContent
              else say "Not updating sources.nix"
        ),
        ( pathNixSourcesJson fsj,
          \path -> do
            createFile path initNixSourcesJsonContent

            -- Import nixpkgs, if necessary
            initNixpkgs nixpkgs,
          \path _content -> dontCreateFile path
        )
      ]
      $ \(path, onCreate, onUpdate) -> do
        exists <- li $ Dir.doesFileExist path
        if exists then li (B.readFile path) >>= onUpdate path else onCreate path
    case fsj of
      Auto -> pure ()
      AtPath fp ->
        tsay $
          T.unlines
            [ T.unwords
                [ tbold $ tblue "INFO:",
                  "You are using a custom path for sources.json."
                ],
              "  You need to configure the sources.nix to use " <> tbold (T.pack fp) <> ":",
              tbold "      import sources.nix { sourcesFile = PATH ; }; ",
              T.unwords
                [ "  where",
                  tbold "PATH",
                  "is the relative path from sources.nix to",
                  tbold (T.pack fp) <> "."
                ]
            ]
  where
    createFile :: FilePath -> B.ByteString -> NIO ()
    createFile path content = li $ do
      let dir = takeDirectory path
      Dir.createDirectoryIfMissing True dir
      say $ "Creating " <> path
      B.writeFile path content
    dontCreateFile :: FilePath -> NIO ()
    dontCreateFile path = say $ "Not creating " <> path

initNixpkgs :: FetchNixpkgs -> NIO ()
initNixpkgs nixpkgs =
  case nixpkgs of
    NoNixpkgs -> say "Not importing 'nixpkgs'."
    NixpkgsFast -> do
      say "Using known 'nixpkgs' ..."
      packageSpec <- HTTP.getResponseBody <$> HTTP.httpJSON "https://raw.githubusercontent.com/nmattia/niv/master/data/nixpkgs.json"
      cmdAdd
        githubCmd
        (PackageName "nixpkgs")
        (specToLockedAttrs packageSpec)
      pure ()
    NixpkgsCustom branch nixpkgs' -> do
      say "Importing 'nixpkgs' ..."
      let (owner, repo) = case nixpkgs' of
            Nixpkgs o r -> (o, r)
      cmdAdd
        githubCmd
        (PackageName "nixpkgs")
        ( specToFreeAttrs $
            PackageSpec $
              KM.fromList
                [ "owner" .= owner,
                  "repo" .= repo,
                  "branch" .= branch
                ]
        )

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
    parseShortcut cmd = uncurry (cmdAdd cmd) <$> parseShortcutArgs cmd
    parseCmd cmd = uncurry (cmdAdd cmd) <$> parseCmdArgs cmd
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

cmdAdd :: Cmd -> PackageName -> Attrs -> NIO ()
cmdAdd cmd packageName attrs = do
  job ("Adding package " <> T.unpack (unPackageName packageName)) $ do
    fsj <- getFindSourcesJson
    sources <- unSources <$> li (getSources fsj)
    when (HMS.member packageName sources) $
      li $
        abortCannotAddPackageExists packageName
    eFinalSpec <- fmap attrsToSpec <$> li (doUpdate attrs cmd)
    case eFinalSpec of
      Left e -> li (abortUpdateFailed [(packageName, e)])
      Right finalSpec -> do
        say "Writing new sources file"
        li $
          setSources fsj $
            Sources $
              HMS.insert packageName finalSpec sources

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

cmdUpdate :: Maybe (PackageName, PackageSpec) -> NIO ()
cmdUpdate = \case
  Just (packageName, cliSpec) ->
    job ("Update " <> T.unpack (unPackageName packageName)) $ do
      fsj <- getFindSourcesJson
      sources <- unSources <$> li (getSources fsj)
      eFinalSpec <- case HMS.lookup packageName sources of
        Just defaultSpec -> do
          -- lookup the "type" to find a Cmd to run, defaulting to legacy
          -- github
          let cmd = case KM.lookup "type" (unPackageSpec defaultSpec) of
                Just "git" -> gitCmd
                Just "local" -> localCmd
                _ -> githubCmd
              spec = specToLockedAttrs cliSpec <> specToFreeAttrs defaultSpec
          fmap attrsToSpec <$> li (doUpdate spec cmd)
        Nothing -> li $ abortCannotUpdateNoSuchPackage packageName
      case eFinalSpec of
        Left e -> li $ abortUpdateFailed [(packageName, e)]
        Right finalSpec ->
          li $
            setSources fsj $
              Sources $
                HMS.insert packageName finalSpec sources
  Nothing -> job "Updating all packages" $ do
    fsj <- getFindSourcesJson
    sources <- unSources <$> li (getSources fsj)
    esources' <- forWithKeyM sources $
      \packageName defaultSpec -> do
        tsay $ "Package: " <> unPackageName packageName
        let initialSpec = specToFreeAttrs defaultSpec
        -- lookup the "type" to find a Cmd to run, defaulting to legacy
        -- github
        let cmd = case KM.lookup "type" (unPackageSpec defaultSpec) of
              Just "git" -> gitCmd
              Just "local" -> localCmd
              _ -> githubCmd
        fmap attrsToSpec <$> li (doUpdate initialSpec cmd)
    let (failed, sources') = partitionEithersHMS esources'
    unless (HMS.null failed) $
      li $
        abortUpdateFailed (HMS.toList failed)
    li $ setSources fsj $ Sources sources'

-- | pretty much tryEvalUpdate but we might issue some warnings first
doUpdate :: Attrs -> Cmd -> IO (Either SomeException Attrs)
doUpdate attrs cmd = do
  forM_ (extraLogs cmd attrs) tsay
  tryEvalUpdate attrs (updateCmd cmd)

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
  tsay $ "Modifying package: " <> unPackageName packageName
  fsj <- getFindSourcesJson
  sources <- unSources <$> li (getSources fsj)
  finalSpec <- case HMS.lookup packageName sources of
    Just defaultSpec -> pure $ attrsToSpec (specToLockedAttrs cliSpec <> specToFreeAttrs defaultSpec)
    Nothing -> li $ abortCannotModifyNoSuchPackage packageName
  case mNewName of
    Just newName -> do
      when (HMS.member newName sources) $
        li $
          abortCannotAddPackageExists newName
      li $ setSources fsj $ Sources $ HMS.insert newName finalSpec $ HMS.delete packageName sources
    Nothing ->
      li $ setSources fsj $ Sources $ HMS.insert packageName finalSpec sources

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
    tsay $ "Dropping package: " <> unPackageName packageName
    fsj <- getFindSourcesJson
    sources <- unSources <$> li (getSources fsj)
    unless (HMS.member packageName sources) $
      li $
        abortCannotDropNoSuchPackage packageName
    li $
      setSources fsj $
        Sources $
          HMS.delete packageName sources
  attrs -> do
    tsay $ "Dropping attributes: " <> T.intercalate " " attrs
    tsay $ "In package: " <> unPackageName packageName
    fsj <- getFindSourcesJson
    sources <- unSources <$> li (getSources fsj)
    packageSpec <- case HMS.lookup packageName sources of
      Nothing ->
        li $ abortCannotAttributesDropNoSuchPackage packageName
      Just (PackageSpec packageSpec) ->
        pure $
          PackageSpec $
            KM.mapMaybeWithKey
              (\k v -> if K.toText k `elem` attrs then Nothing else Just v)
              packageSpec
    li $
      setSources fsj $
        Sources $
          HMS.insert packageName packageSpec sources

-------------------------------------------------------------------------------
-- Files and their content
-------------------------------------------------------------------------------

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
-- Abort
-------------------------------------------------------------------------------

abortCannotAddPackageExists :: PackageName -> IO a
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

abortCannotUpdateNoSuchPackage :: PackageName -> IO a
abortCannotUpdateNoSuchPackage (PackageName n) =
  abort $
    T.unlines
      [ "Cannot update package " <> n <> ".",
        "The package doesn't exist. Use",
        "  niv add " <> n,
        "to add the package."
      ]

abortCannotModifyNoSuchPackage :: PackageName -> IO a
abortCannotModifyNoSuchPackage (PackageName n) =
  abort $
    T.unlines
      [ "Cannot modify package " <> n <> ".",
        "The package doesn't exist. Use",
        "  niv add " <> n,
        "to add the package."
      ]

abortCannotDropNoSuchPackage :: PackageName -> IO a
abortCannotDropNoSuchPackage (PackageName n) =
  abort $
    T.unlines
      [ "Cannot drop package " <> n <> ".",
        "The package doesn't exist."
      ]

abortCannotShowNoSuchPackage :: PackageName -> IO a
abortCannotShowNoSuchPackage (PackageName n) =
  abort $
    T.unlines
      [ "Cannot show package " <> n <> ".",
        "The package doesn't exist."
      ]

abortCannotAttributesDropNoSuchPackage :: PackageName -> IO a
abortCannotAttributesDropNoSuchPackage (PackageName n) =
  abort $
    T.unlines
      [ "Cannot drop attributes of package " <> n <> ".",
        "The package doesn't exist."
      ]

abortUpdateFailed :: [(PackageName, SomeException)] -> IO a
abortUpdateFailed errs =
  abort $
    T.unlines $
      ["One or more packages failed to update:"]
        <> map
          ( \(PackageName pname, e) ->
              pname <> ": " <> tshow e
          )
          errs
