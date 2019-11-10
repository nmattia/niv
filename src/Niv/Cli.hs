{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Niv.Cli where

import Control.Applicative
import Control.Monad
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey, (.=))
import Data.Char (isSpace)
import Data.FileEmbed (embedFile)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.String.QQ (s)
import Niv.Logger
import Niv.GitHub
import Niv.Update
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>), takeDirectory)
import System.Process (readProcessWithExitCode)
import System.Environment (getArgs)
import UnliftIO
import Data.Version (showVersion)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Data.Text.IO as T
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

newtype Sources = Sources
  { unSources :: HMS.HashMap PackageName PackageSpec }
  deriving newtype (FromJSON, ToJSON)

getSources :: IO Sources
getSources = do
    exists <- Dir.doesFileExist pathNixSourcesJson
    unless exists abortSourcesDoesntExist

    warnIfOutdated
    -- TODO: if doesn't exist: run niv init
    say $ "Reading sources file"
    decodeFileStrict pathNixSourcesJson >>= \case
      Just (Aeson.Object obj) ->
        fmap (Sources . mconcat) $
          forM (HMS.toList obj) $ \(k, v) ->
            case v of
              Aeson.Object v' ->
                pure $ HMS.singleton (PackageName k) (PackageSpec v')
              _ -> abortAttributeIsntAMap
      Just _ -> abortSourcesIsntAMap
      Nothing -> abortSourcesIsntJSON

setSources :: Sources -> IO ()
setSources sources = encodeFile pathNixSourcesJson sources

newtype PackageName = PackageName { unPackageName :: T.Text }
  deriving newtype (Eq, Hashable, FromJSONKey, ToJSONKey, Show)

parsePackageName :: Opts.Parser PackageName
parsePackageName = PackageName <$>
    Opts.argument Opts.str (Opts.metavar "PACKAGE")

newtype PackageSpec = PackageSpec { unPackageSpec :: Aeson.Object }
  deriving newtype (FromJSON, ToJSON, Show, Semigroup, Monoid)

-- | Simply discards the 'Freedom'
attrsToSpec :: Attrs -> PackageSpec
attrsToSpec = PackageSpec . fmap snd

parsePackageSpec :: Opts.Parser PackageSpec
parsePackageSpec =
    (PackageSpec . HMS.fromList . fmap fixupAttributes) <$>
      many parseAttribute
  where
    parseAttribute :: Opts.Parser (T.Text, T.Text)
    parseAttribute =
      Opts.option (Opts.maybeReader parseKeyVal)
        ( Opts.long "attribute" <>
          Opts.short 'a' <>
          Opts.metavar "KEY=VAL" <>
          Opts.help "Set the package spec attribute <KEY> to <VAL>"
        ) <|> shortcutAttributes <|>
      (("url_template",) <$> Opts.strOption
        ( Opts.long "template" <>
          Opts.short 't' <>
          Opts.metavar "URL" <>
          Opts.help "Used during 'update' when building URL. Occurrences of <foo> are replaced with attribute 'foo'."
        )) <|>
      (("type",) <$> Opts.strOption
        ( Opts.long "type" <>
          Opts.short 'T' <>
          Opts.metavar "TYPE" <>
          Opts.help "The type of the URL target. The value can be either 'file' or 'tarball'. If not set, the value is inferred from the suffix of the URL."
        ))

    -- Parse "key=val" into ("key", "val")
    parseKeyVal :: String -> Maybe (T.Text, T.Text)
    parseKeyVal str = case span (/= '=') str of
      (key, '=':val) -> Just (T.pack key, T.pack val)
      _ -> Nothing

    -- Shortcuts for common attributes
    shortcutAttributes :: Opts.Parser (T.Text, T.Text)
    shortcutAttributes = foldr (<|>) empty $ mkShortcutAttribute <$>
      [ "branch", "owner", "repo", "version" ]

    -- TODO: infer those shortcuts from 'Update' keys
    mkShortcutAttribute :: T.Text -> Opts.Parser (T.Text, T.Text)
    mkShortcutAttribute = \case
      attr@(T.uncons -> Just (c,_)) -> (attr,) <$> Opts.strOption
        ( Opts.long (T.unpack attr) <>
          Opts.short c <>
          Opts.metavar (T.unpack $ T.toUpper attr) <>
          Opts.help
            ( T.unpack $
              "Equivalent to --attribute " <>
              attr <> "=<" <> (T.toUpper attr) <> ">"
            )
        )
      _ -> empty

    fixupAttributes :: (T.Text, T.Text) -> (T.Text, Aeson.Value)
    fixupAttributes (k, v) = (k, Aeson.String v)

parsePackage :: Opts.Parser (PackageName, PackageSpec)
parsePackage = (,) <$> parsePackageName <*> parsePackageSpec

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
              cmdAdd Nothing (PackageName "nmattia/niv", PackageSpec HMS.empty)
              say "Importing 'nixpkgs' ..."
              cmdAdd
                (Just (PackageName "nixpkgs"))
                ( PackageName "NixOS/nixpkgs-channels"
                , PackageSpec (HMS.singleton "branch" "nixos-19.03"))
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
    Opts.info ((cmdAdd <$> optName <*> parsePackage) <**> Opts.helper) $
      mconcat desc
  where
    optName :: Opts.Parser (Maybe PackageName)
    optName = Opts.optional $ PackageName <$>  Opts.strOption
      ( Opts.long "name" <>
        Opts.short 'n' <>
        Opts.metavar "NAME" <>
        Opts.help "Set the package name to <NAME>"
      )
    desc =
      [ Opts.fullDesc
      , Opts.progDesc "Add dependency"
      , Opts.headerDoc $ Just $
          "Examples:" Opts.<$$>
          "" Opts.<$$>
          "  niv add stedolan/jq" Opts.<$$>
          "  niv add NixOS/nixpkgs-channels -n nixpkgs -b nixos-19.03" Opts.<$$>
          "  niv add my-package -v alpha-0.1 -t http://example.com/archive/<version>.zip"
      ]

cmdAdd :: Maybe PackageName -> (PackageName, PackageSpec) -> IO ()
cmdAdd mPackageName (PackageName str, cliSpec) =
    job ("Adding package " <> T.unpack str)  $ do

      -- Figures out the owner and repo
      let (packageName, defaultSpec) = case T.span (/= '/') str of
            ( owner@(T.null -> False)
              , T.uncons -> Just ('/', repo@(T.null -> False))) -> do
              (PackageName repo, HMS.fromList [ "owner" .= owner, "repo" .= repo ])
            _ -> (PackageName str, HMS.empty)

      sources <- unSources <$> getSources

      let packageName' = fromMaybe packageName mPackageName

      when (HMS.member packageName' sources) $
        abortCannotAddPackageExists packageName'

      let defaultSpec' = PackageSpec $ defaultSpec

      let initialSpec = specToLockedAttrs cliSpec <> specToFreeAttrs defaultSpec'

      eFinalSpec <- fmap attrsToSpec <$> tryEvalUpdate
        initialSpec
        (githubUpdate nixPrefetchURL githubLatestRev githubRepo)

      case eFinalSpec of
        Left e -> abortUpdateFailed [(packageName', e)]
        Right finalSpec -> do
          say $ "Writing new sources file"
          setSources $ Sources $
            HMS.insert packageName' finalSpec sources

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
      , Opts.headerDoc $ Just $
          "Examples:" Opts.<$$>
          "" Opts.<$$>
          "  niv update" Opts.<$$>
          "  niv update nixpkgs" Opts.<$$>
          "  niv update my-package -v beta-0.2"
      ]

specToFreeAttrs :: PackageSpec -> Attrs
specToFreeAttrs = fmap (Free,) . unPackageSpec

specToLockedAttrs :: PackageSpec -> Attrs
specToLockedAttrs = fmap (Locked,) . unPackageSpec

-- TODO: sexy logging + concurrent updates
cmdUpdate :: Maybe (PackageName, PackageSpec) -> IO ()
cmdUpdate = \case
    Just (packageName, cliSpec) ->
      job ("Update " <> T.unpack (unPackageName packageName)) $ do
        sources <- unSources <$> getSources

        eFinalSpec <- case HMS.lookup packageName sources of
          Just defaultSpec -> do
            fmap attrsToSpec <$> tryEvalUpdate
              (specToLockedAttrs cliSpec <> specToFreeAttrs defaultSpec)
              (githubUpdate nixPrefetchURL githubLatestRev githubRepo)

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
            (githubUpdate nixPrefetchURL githubLatestRev githubRepo)
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
-- Aux
-------------------------------------------------------------------------------

--- Aeson

-- | Efficiently deserialize a JSON value from a file.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- The input file's content must consist solely of a JSON document,
-- with no trailing data except for whitespace.
--
-- This function parses immediately, but defers conversion.  See
-- 'json' for details.
decodeFileStrict :: (FromJSON a) => FilePath -> IO (Maybe a)
decodeFileStrict = fmap Aeson.decodeStrict . B.readFile

-- | Efficiently serialize a JSON value as a lazy 'L.ByteString' and write it to a file.
encodeFile :: (ToJSON a) => FilePath -> a -> IO ()
encodeFile fp = L.writeFile fp . AesonPretty.encodePretty' config
  where
    config =  AesonPretty.defConfig { AesonPretty.confTrailingNewline = True, AesonPretty.confCompare = compare }

--- HashMap

forWithKeyM
  :: (Eq k, Hashable k, Monad m)
  => HMS.HashMap k v1
  -> (k -> v1 -> m v2)
  -> m (HMS.HashMap k v2)
forWithKeyM = flip mapWithKeyM

forWithKeyM_
  :: (Eq k, Hashable k, Monad m)
  => HMS.HashMap k v1
  -> (k -> v1 -> m ())
  -> m ()
forWithKeyM_ = flip mapWithKeyM_

mapWithKeyM
  :: (Eq k, Hashable k, Monad m)
  => (k -> v1 -> m v2)
  -> HMS.HashMap k v1
  -> m (HMS.HashMap k v2)
mapWithKeyM f m = do
    fmap mconcat $ forM (HMS.toList m) $ \(k, v) ->
      HMS.singleton k <$> f k v

mapWithKeyM_
  :: (Eq k, Hashable k, Monad m)
  => (k -> v1 -> m ())
  -> HMS.HashMap k v1
  -> m ()
mapWithKeyM_ f m = do
    forM_ (HMS.toList m) $ \(k, v) ->
      HMS.singleton k <$> f k v

nixPrefetchURL :: Bool -> T.Text -> IO T.Text
nixPrefetchURL unpack (T.unpack -> url) = do
    (exitCode, sout, serr) <- runNixPrefetch
    case (exitCode, lines sout) of
      (ExitSuccess, l:_)  -> pure $ T.pack l
      _ -> abortNixPrefetchExpectedOutput (T.pack sout) (T.pack serr)
  where
    args = if unpack then ["--unpack", url] else [url]
    runNixPrefetch = readProcessWithExitCode "nix-prefetch-url" args ""

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

warnIfOutdated :: IO ()
warnIfOutdated = do
    tryAny (B.readFile pathNixSourcesNix) >>= \case
      Left e -> T.putStrLn $ T.unlines
        [ "Could not read " <> T.pack pathNixSourcesNix
        , "Error: " <> tshow e
        ]
      Right content ->
        if shouldUpdateNixSourcesNix content
        then
          T.putStrLn $ T.unlines
            [ "WARNING: " <> T.pack pathNixSourcesNix <> " is out of date."
            , "Please run"
            , "  niv init"
            , "or add the following line in the " <> T.pack pathNixSourcesNix <> "  file:"
            , "  # niv: no_update"
            ]
        else pure ()

-- | @nix/sources.nix@
pathNixSourcesNix :: FilePath
pathNixSourcesNix = "nix" </> "sources.nix"

-- | Glue code between nix and sources.json
initNixSourcesNixContent :: B.ByteString
initNixSourcesNixContent = $(embedFile "nix/sources.nix")

-- | @nix/sources.json"
pathNixSourcesJson :: FilePath
pathNixSourcesJson = "nix" </> "sources.json"

-- | Empty JSON map
initNixSourcesJsonContent :: B.ByteString
initNixSourcesJsonContent = "{}"

-------------------------------------------------------------------------------
-- Abort
-------------------------------------------------------------------------------

abortSourcesDoesntExist :: IO a
abortSourcesDoesntExist = abort $ T.unlines [ line1, line2 ]
  where
    line1 = "Cannot use " <> T.pack pathNixSourcesJson
    line2 = [s|
The sources file does not exist! You may need to run 'niv init'.
|]

abortSourcesIsntAMap :: IO a
abortSourcesIsntAMap = abort $ T.unlines [ line1, line2 ]
  where
    line1 = "Cannot use " <> T.pack pathNixSourcesJson
    line2 = [s|
The sources file should be a JSON map from package name to package
specification, e.g.:
  { ... }
|]

abortAttributeIsntAMap :: IO a
abortAttributeIsntAMap = abort $ T.unlines [ line1, line2 ]
  where
    line1 = "Cannot use " <> T.pack pathNixSourcesJson
    line2 = [s|
The package specifications in the sources file should be JSON maps from
attribute name to attribute value, e.g.:
  { "nixpkgs": { "foo": "bar" } }
|]

abortSourcesIsntJSON :: IO a
abortSourcesIsntJSON = abort $ T.unlines [ line1, line2 ]
  where
    line1 = "Cannot use " <> T.pack pathNixSourcesJson
    line2 = "The sources file should be JSON."

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

abortNixPrefetchExpectedOutput :: T.Text -> T.Text -> IO a
abortNixPrefetchExpectedOutput sout serr = abort $ [s|
Could not read the output of 'nix-prefetch-url'. This is a bug. Please create a
ticket:

  https://github.com/nmattia/niv/issues/new

Thanks! I'll buy you a beer.
|] <> T.unlines ["stdout: ", sout, "stderr: ", serr]
