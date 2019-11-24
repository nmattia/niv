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
import Data.Aeson ((.=))
import Data.Char (isSpace)
import Data.HashMap.Strict.Extended
import Data.Hashable (Hashable)
import Data.String.QQ (s)
import Data.Text.Extended
import Data.Version (showVersion)
import Niv.Cmd
import Niv.Git.Cmd
import Niv.GitHub.Cmd
import Niv.Logger
import Niv.Sources
import Niv.Update
import System.Environment (getArgs)
import System.FilePath (takeDirectory)
import UnliftIO
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Help.Pretty as Opts
import qualified System.Directory as Dir

-- I died a little
import Paths_niv (version)

cli :: IO ()
cli = join $
    execParserPure' Opts.defaultPrefs opts <$> getArgs
      >>= Opts.handleParseResult
  where
    execParserPure' pprefs pinfo [] = Opts.Failure $
      Opts.parserFailure pprefs pinfo Opts.ShowHelpText mempty
    execParserPure' pprefs pinfo args = Opts.execParserPure pprefs pinfo args
    opts = Opts.info (parseCommand <**> Opts.helper ) $ mconcat desc
    desc =
      [ Opts.fullDesc
      , Opts.headerDoc $ Just $
          "niv - dependency manager for Nix projects" Opts.<$$>
          "" Opts.<$$>
          "version:" Opts.<+> Opts.text (showVersion version)
      ]

parseCommand :: Opts.Parser (IO ())
parseCommand = Opts.subparser (
    Opts.command "init" parseCmdInit <>
    Opts.command "add"  parseCmdAdd <>
    Opts.command "show"  parseCmdShow <>
    Opts.command "update"  parseCmdUpdate <>
    Opts.command "modify"  parseCmdModify <>
    Opts.command "drop"  parseCmdDrop )

parsePackageName :: Opts.Parser PackageName
parsePackageName = PackageName <$>
    Opts.argument Opts.str (Opts.metavar "PACKAGE")

parsePackage :: Opts.Parser (PackageName, PackageSpec)
parsePackage = (,) <$> parsePackageName <*> (parsePackageSpec githubCmd)

-------------------------------------------------------------------------------
-- INIT
-------------------------------------------------------------------------------

parseCmdInit :: Opts.ParserInfo (IO ())
parseCmdInit = Opts.info (pure cmdInit <**> Opts.helper) $ mconcat desc
  where
    desc =
      [ Opts.fullDesc
      , Opts.progDesc
          "Initialize a Nix project. Existing files won't be modified."
      ]

cmdInit :: IO ()
cmdInit = do
    job "Initializing" $ do

      -- Writes all the default files
      -- a path, a "create" function and an update function for each file.
      forM_
        [ ( pathNixSourcesNix
          , (`createFile` initNixSourcesNixContent)
          , \path content -> do
              if shouldUpdateNixSourcesNix content
              then do
                say "Updating sources.nix"
                B.writeFile path initNixSourcesNixContent
              else say "Not updating sources.nix"
          )
        , ( pathNixSourcesJson
          , \path -> do
              createFile path initNixSourcesJsonContent
              -- Imports @niv@ and @nixpkgs@ (19.03)
              say "Importing 'niv' ..."
              cmdAdd (updateCmd githubCmd) (PackageName "niv")
                (specToFreeAttrs $ PackageSpec $ HMS.fromList
                  [ "owner" .= ("nmattia" :: T.Text)
                  , "repo" .= ("niv" :: T.Text)
                  ]
                )
              say "Importing 'nixpkgs' ..."
              cmdAdd (updateCmd githubCmd) (PackageName "nixpkgs")
                (specToFreeAttrs $ PackageSpec $ HMS.fromList
                  [ "owner" .= ("NixOS" :: T.Text)
                  , "repo" .= ("nixpkgs-channels" :: T.Text)
                  , "branch" .= ("nixos-19.03" :: T.Text)
                  ]
                )
          , \path _content -> dontCreateFile path)
        ] $ \(path, onCreate, onUpdate) -> do
            exists <- Dir.doesFileExist path
            if exists then B.readFile path >>= onUpdate path else onCreate path
  where
    createFile :: FilePath -> B.ByteString -> IO ()
    createFile path content = do
      let dir = takeDirectory path
      Dir.createDirectoryIfMissing True dir
      say $ "Creating " <> path
      B.writeFile path content
    dontCreateFile :: FilePath -> IO ()
    dontCreateFile path = say $ "Not creating " <> path

-------------------------------------------------------------------------------
-- ADD
-------------------------------------------------------------------------------

parseCmdAdd :: Opts.ParserInfo (IO ())
parseCmdAdd =
    Opts.info
      ((parseCommands <|> parseShortcuts) <**> Opts.helper) $
      (description githubCmd)
  where
    -- XXX: this should parse many shortcuts (github, git). Right now we only
    -- parse GitHub because the git interface is still experimental.  note to
    -- implementer: it'll be tricky to have the correct arguments show up
    -- without repeating "PACKAGE PACKAGE PACKAGE" for every package type.
    parseShortcuts = parseShortcut githubCmd
    parseShortcut cmd = uncurry (cmdAdd (updateCmd cmd)) <$> (parseShortcutArgs cmd)
    parseCmd cmd = uncurry (cmdAdd (updateCmd cmd)) <$> (parseCmdArgs cmd)
    parseCmdAddGit =
      Opts.info (parseCmd gitCmd <**> Opts.helper) (description gitCmd)
    parseCmdAddGitHub =
      Opts.info (parseCmd githubCmd <**> Opts.helper) (description githubCmd)
    parseCommands = Opts.subparser
        ( Opts.hidden <>
          Opts.commandGroup "Experimental commands:" <>
          Opts.command "git" parseCmdAddGit <>
          Opts.command "github" parseCmdAddGitHub
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
      (,) <$>
        Opts.argument
          (Opts.maybeReader (parseCmdShortcut cmd . T.pack))
          (Opts.metavar "PACKAGE") <*>
        optName
    optName = Opts.optional $ PackageName <$> Opts.strOption
      ( Opts.long "name" <>
        Opts.short 'n' <>
        Opts.metavar "NAME" <>
        Opts.help "Set the package name to <NAME>"
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
          (Nothing, Just pname') -> (pname', PackageSpec HMS.empty)
          (Nothing, Nothing) -> (PackageName "unnamed", PackageSpec HMS.empty)
    parseNameAndShortcut =
      (,) <$>
        Opts.optional (Opts.argument
          (Opts.maybeReader (parseCmdShortcut cmd . T.pack))
          (Opts.metavar "PACKAGE")) <*>
        optName
    optName = Opts.optional $ PackageName <$> Opts.strOption
      ( Opts.long "name" <>
        Opts.short 'n' <>
        Opts.metavar "NAME" <>
        Opts.help "Set the package name to <NAME>"
      )

cmdAdd :: Update () a -> PackageName -> Attrs -> IO ()
cmdAdd updateFunc packageName attrs = do
    job ("Adding package " <> T.unpack (unPackageName packageName)) $ do
      sources <- unSources <$> getSources

      when (HMS.member packageName sources) $
        abortCannotAddPackageExists packageName

      eFinalSpec <- fmap attrsToSpec <$> tryEvalUpdate attrs updateFunc

      case eFinalSpec of
        Left e -> abortUpdateFailed [(packageName, e)]
        Right finalSpec -> do
          say $ "Writing new sources file"
          setSources $ Sources $
            HMS.insert packageName finalSpec sources

-------------------------------------------------------------------------------
-- SHOW
-------------------------------------------------------------------------------

parseCmdShow :: Opts.ParserInfo (IO ())
parseCmdShow =
    Opts.info
      ((cmdShow <$> Opts.optional parsePackageName) <**> Opts.helper)
      Opts.fullDesc

-- TODO: nicer output
cmdShow :: Maybe PackageName -> IO ()
cmdShow = \case
    Just packageName -> do
      sources <- unSources <$> getSources

      case HMS.lookup packageName sources of
        Just pspec -> showPackage packageName pspec
        Nothing -> abortCannotShowNoSuchPackage packageName

    Nothing -> do
      sources <- unSources <$> getSources
      forWithKeyM_ sources $ showPackage

showPackage :: PackageName -> PackageSpec -> IO ()
showPackage (PackageName pname) (PackageSpec spec) = do
    tsay $ tbold pname
    forM_ (HMS.toList spec) $ \(attrName, attrValValue) -> do
      let attrValue = case attrValValue of
            Aeson.String str -> str
            _ -> tfaint "<barabajagal>"
      tsay $ "  " <> attrName <> ": " <> attrValue

-------------------------------------------------------------------------------
-- UPDATE
-------------------------------------------------------------------------------

parseCmdUpdate :: Opts.ParserInfo (IO ())
parseCmdUpdate =
    Opts.info
      ((cmdUpdate <$> Opts.optional parsePackage) <**> Opts.helper) $
      mconcat desc
  where
    desc =
      [ Opts.fullDesc
      , Opts.progDesc "Update dependencies"
      , Opts.headerDoc $ Just $ Opts.nest 2 $
          "Examples:" Opts.<$$>
          "" Opts.<$$>
          Opts.vcat
            [ Opts.fill 30 "niv update" Opts.<+> "# update all packages",
              Opts.fill 30 "niv update nixpkgs" Opts.<+> "# update nixpkgs",
              Opts.fill 30 "niv update my-package -v beta-0.2" Opts.<+> "# update my-package to version \"beta-0.2\""
            ]
      ]

specToFreeAttrs :: PackageSpec -> Attrs
specToFreeAttrs = fmap (Free,) . unPackageSpec

specToLockedAttrs :: PackageSpec -> Attrs
specToLockedAttrs = fmap (Locked,) . unPackageSpec

cmdUpdate :: Maybe (PackageName, PackageSpec) -> IO ()
cmdUpdate = \case
    Just (packageName, cliSpec) ->
      job ("Update " <> T.unpack (unPackageName packageName)) $ do
        sources <- unSources <$> getSources

        eFinalSpec <- case HMS.lookup packageName sources of
          Just defaultSpec -> do
            fmap attrsToSpec <$> tryEvalUpdate
              (specToLockedAttrs cliSpec <> specToFreeAttrs defaultSpec)
              (updateCmd githubCmd)

          Nothing -> abortCannotUpdateNoSuchPackage packageName

        case eFinalSpec of
          Left e -> abortUpdateFailed [(packageName, e)]
          Right finalSpec ->
            setSources $ Sources $
              HMS.insert packageName finalSpec sources

    Nothing -> job "Updating all packages" $ do
      sources <- unSources <$> getSources

      esources' <- forWithKeyM sources $
        \packageName defaultSpec -> do
          tsay $ "Package: " <> unPackageName packageName
          let initialSpec = specToFreeAttrs defaultSpec
          finalSpec <- fmap attrsToSpec <$> tryEvalUpdate
            initialSpec
            (updateCmd githubCmd)
          pure finalSpec

      let (failed, sources') = partitionEithersHMS esources'

      unless (HMS.null failed) $
        abortUpdateFailed (HMS.toList failed)

      setSources $ Sources sources'

partitionEithersHMS
  :: (Eq k, Hashable k)
  => HMS.HashMap k (Either a b) -> (HMS.HashMap k a, HMS.HashMap k b)
partitionEithersHMS =
    flip HMS.foldlWithKey' (HMS.empty, HMS.empty) $ \(ls, rs) k -> \case
      Left l -> (HMS.insert k l ls, rs)
      Right r -> (ls, HMS.insert k r rs)

-------------------------------------------------------------------------------
-- MODIFY
-------------------------------------------------------------------------------

parseCmdModify :: Opts.ParserInfo (IO ())
parseCmdModify =
    Opts.info
      ((cmdModify <$> parsePackage) <**> Opts.helper) $
      mconcat desc
  where
    desc =
      [ Opts.fullDesc
      , Opts.progDesc "Modify dependency"
      , Opts.headerDoc $ Just $
          "Examples:" Opts.<$$>
          "" Opts.<$$>
          "  niv modify nixpkgs -v beta-0.2" Opts.<$$>
          "  niv modify nixpkgs -a branch=nixpkgs-unstable"
      ]

cmdModify :: (PackageName, PackageSpec) -> IO ()
cmdModify (packageName, cliSpec) = do
    tsay $ "Modifying package: " <> unPackageName packageName
    sources <- unSources <$> getSources

    finalSpec <- case HMS.lookup packageName sources of
      Just defaultSpec -> pure $ attrsToSpec (specToLockedAttrs cliSpec <> specToFreeAttrs defaultSpec)
      Nothing -> abortCannotModifyNoSuchPackage packageName

    setSources $ Sources $ HMS.insert packageName finalSpec sources

-------------------------------------------------------------------------------
-- DROP
-------------------------------------------------------------------------------

parseCmdDrop :: Opts.ParserInfo (IO ())
parseCmdDrop =
    Opts.info
      ((cmdDrop <$> parsePackageName <*> parseDropAttributes) <**>
        Opts.helper) $
      mconcat desc
  where
    desc =
      [ Opts.fullDesc
      , Opts.progDesc "Drop dependency"
      , Opts.headerDoc $ Just $
          "Examples:" Opts.<$$>
          "" Opts.<$$>
          "  niv drop jq" Opts.<$$>
          "  niv drop my-package version"
      ]
    parseDropAttributes :: Opts.Parser [T.Text]
    parseDropAttributes = many $
      Opts.argument Opts.str (Opts.metavar "ATTRIBUTE")

cmdDrop :: PackageName -> [T.Text] -> IO ()
cmdDrop packageName = \case
    [] -> do
      tsay $ "Dropping package: " <> unPackageName packageName
      sources <- unSources <$> getSources

      when (not $ HMS.member packageName sources) $
        abortCannotDropNoSuchPackage packageName

      setSources $ Sources $
        HMS.delete packageName sources
    attrs -> do
      tsay $ "Dropping attributes :" <> T.intercalate " " attrs
      tsay $ "In package: " <> unPackageName packageName
      sources <- unSources <$> getSources

      packageSpec <- case HMS.lookup packageName sources of
        Nothing ->
          abortCannotAttributesDropNoSuchPackage packageName
        Just (PackageSpec packageSpec) -> pure $ PackageSpec $
          HMS.mapMaybeWithKey
            (\k v -> if k `elem` attrs then Nothing else Just v) packageSpec

      setSources $ Sources $
        HMS.insert packageName packageSpec sources

-------------------------------------------------------------------------------
-- Files and their content
-------------------------------------------------------------------------------

-- | Checks if content is different than default and if it does /not/ contain
-- a comment line with @niv: no_update@
shouldUpdateNixSourcesNix :: B.ByteString -> Bool
shouldUpdateNixSourcesNix content =
    content /= initNixSourcesNixContent &&
      not (any lineForbids (B8.lines content))
  where
    lineForbids :: B8.ByteString -> Bool
    lineForbids str =
      case B8.uncons (B8.dropWhile isSpace str) of
        Just ('#',rest) -> case B8.stripPrefix "niv:" (B8.dropWhile isSpace rest) of
          Just rest' -> case B8.stripPrefix "no_update" (B8.dropWhile isSpace rest') of
            Just{} -> True
            _ -> False
          _ -> False
        _ -> False

-------------------------------------------------------------------------------
-- Abort
-------------------------------------------------------------------------------

abortSourcesIsntAMap :: IO a
abortSourcesIsntAMap = abort $ T.unlines [ line1, line2 ]
  where
    line1 = "Cannot use " <> T.pack pathNixSourcesJson
    line2 = [s|
The sources file should be a JSON map from package name to package
specification, e.g.:
  { ... }
|]

abortCannotAddPackageExists :: PackageName -> IO a
abortCannotAddPackageExists (PackageName n) = abort $ T.unlines
    [ "Cannot add package " <> n <> "."
    , "The package already exists. Use"
    , "  niv drop " <> n
    , "and then re-add the package. Alternatively use"
    , "  niv update " <> n <> " --attribute foo=bar"
    , "to update the package's attributes."
    ]

abortCannotUpdateNoSuchPackage :: PackageName -> IO a
abortCannotUpdateNoSuchPackage (PackageName n) = abort $ T.unlines
    [ "Cannot update package " <> n <> "."
    , "The package doesn't exist. Use"
    , "  niv add " <> n
    , "to add the package."
    ]

abortCannotModifyNoSuchPackage :: PackageName -> IO a
abortCannotModifyNoSuchPackage (PackageName n) = abort $ T.unlines
    [ "Cannot modify package " <> n <> "."
    , "The package doesn't exist. Use"
    , "  niv add " <> n
    , "to add the package."
    ]

abortCannotDropNoSuchPackage :: PackageName -> IO a
abortCannotDropNoSuchPackage (PackageName n) = abort $ T.unlines
    [ "Cannot drop package " <> n <> "."
    , "The package doesn't exist."
    ]

abortCannotShowNoSuchPackage :: PackageName -> IO a
abortCannotShowNoSuchPackage (PackageName n) = abort $ T.unlines
    [ "Cannot show package " <> n <> "."
    , "The package doesn't exist."
    ]

abortCannotAttributesDropNoSuchPackage :: PackageName -> IO a
abortCannotAttributesDropNoSuchPackage (PackageName n) = abort $ T.unlines
    [ "Cannot drop attributes of package " <> n <> "."
    , "The package doesn't exist."
    ]

abortUpdateFailed :: [ (PackageName, SomeException) ] -> IO a
abortUpdateFailed errs = abort $ T.unlines $
    [ "One or more packages failed to update:" ] <>
    map (\(PackageName pname, e) ->
      pname <> ": " <> tshow e
    ) errs

