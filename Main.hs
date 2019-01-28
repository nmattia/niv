{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC "-Wall" #-}

module Main (main) where

-- TODO: proper errors
-- TODO: document commands

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Char (toUpper)
import Data.Hashable (Hashable)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Semigroup ((<>))
import Data.String.QQ (s)
import GHC.Exts (toList)
import System.FilePath ((</>), takeDirectory)
import System.Process (readProcess)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import qualified GitHub as GH
import qualified GitHub.Data.Name as GH
import qualified Options.Applicative as Opts
import qualified System.Directory as Dir

newtype VersionsSpec = VersionsSpec
  { unVersionsSpec :: HMap.HashMap PackageName PackageSpec }
  deriving newtype (FromJSON, ToJSON)

getVersionsSpec :: IO VersionsSpec
getVersionsSpec = do
    putStrLn $ "Reading versions file"
    decodeFileStrict pathNixVersionsJson >>= \case
      Just (Aeson.Object obj) ->
        fmap (VersionsSpec . mconcat) $
          forM (HMap.toList obj) $ \(k, v) ->
            case v of
              Aeson.Object v' ->
                pure $ HMap.singleton (PackageName (T.unpack k)) (PackageSpec v')
              _ -> error "baaaaz"
      Just _ -> error "foo"
      Nothing -> error "Cannot decode versions"

setVersionsSpec :: VersionsSpec -> IO ()
setVersionsSpec versionsSpec = encodeFile pathNixVersionsJson versionsSpec

newtype PackageName = PackageName { unPackageName :: String }
  deriving newtype (Eq, Hashable, FromJSONKey, ToJSONKey, Show)

parsePackageName :: Opts.Parser PackageName
parsePackageName = PackageName <$>
    Opts.argument Opts.str (Opts.metavar "PACKAGE")

newtype PackageSpec = PackageSpec { unPackageSpec :: Aeson.Object }
  deriving newtype (FromJSON, ToJSON, Show)

parsePackageSpec :: Opts.Parser PackageSpec
parsePackageSpec =
    (PackageSpec . HMap.fromList . fmap fixupAttributes) <$>
      many parseAttribute
  where
    parseAttribute :: Opts.Parser (String, String)
    parseAttribute = shortcutAttributes <|>
      Opts.option (Opts.maybeReader parseKeyVal)
        ( Opts.long "attribute" <>
          Opts.short 'a' <>
          Opts.metavar "KEY=VAL"
        )

    -- Parse "key=val" into ("key", "val")
    parseKeyVal :: String -> Maybe (String, String)
    parseKeyVal str = case span (/= '=') str of
      (key, '=':val) -> Just (key, val)
      _ -> Nothing

    -- Shortcuts for common attributes
    shortcutAttributes :: Opts.Parser (String, String)
    shortcutAttributes = foldr (<|>) empty $ mkShortcutAttribute <$>
      [ "branch", "owner", "repo" ]

    mkShortcutAttribute :: String -> Opts.Parser (String, String)
    mkShortcutAttribute = \case
      attr@(c:_) -> (attr,) <$> Opts.strOption
        ( Opts.long attr <> Opts.short c <> Opts.metavar (toUpper <$> attr) )
      _ -> error "The attribute name should not be an empty string"

    fixupAttributes :: (String, String) -> (T.Text, Aeson.Value)
    fixupAttributes (k, v) = (T.pack k, Aeson.String (T.pack v))

parsePackage :: Opts.Parser (PackageName, PackageSpec)
parsePackage = (,) <$> parsePackageName <*> parsePackageSpec

-------------------------------------------------------------------------------
-- PACKAGE SPEC OPS
-------------------------------------------------------------------------------

updatePackageSpec :: PackageSpec -> IO PackageSpec
updatePackageSpec = execStateT $ do
    -- Figures out the URL from the template
    withPackageSpecAttr "url_template" (\case
      Aeson.String (T.unpack -> template) -> do
        packageSpec <- get
        let stringValues = packageSpecStringValues packageSpec
        case renderTemplate stringValues template of
          Just renderedURL ->
            setPackageSpecAttr "url" (Aeson.String $ T.pack renderedURL)
          Nothing -> pure ()
      _ -> pure ()
      )

    -- Updates the sha256 based on the URL contents
    withPackageSpecAttr "url" (\case
      Aeson.String (T.unpack -> url) -> do
        sha256 <- liftIO $ nixPrefetchURL url
        setPackageSpecAttr "sha256" (Aeson.String $ T.pack sha256)
      _ -> pure ()
      )

completePackageSpec
  :: PackageSpec
  -> IO (PackageSpec)
completePackageSpec = execStateT $ do

    -- In case we have @owner@ and @repo@, pull some data from GitHub
    (,) <$> getPackageSpecAttr "owner" <*> getPackageSpecAttr "repo" >>= \case
      (Just (Aeson.String owner), Just (Aeson.String repo)) -> do
          liftIO (GH.executeRequest' $ GH.repositoryR (GH.N owner) (GH.N repo))
            >>= \case
              Left _ -> pure ()
              Right ghRepo -> do

                -- Description
                whenNotSet "description" $ case GH.repoDescription ghRepo of
                  Just descr ->
                    setPackageSpecAttr "description" (Aeson.String descr)
                  Nothing -> pure ()

                -- Branch and rev
                whenNotSet "branch" $ case GH.repoDefaultBranch ghRepo of
                  Just branch ->
                    setPackageSpecAttr "branch" (Aeson.String branch)
                  Nothing -> pure ()

                withPackageSpecAttr "branch" (\case
                  Aeson.String branch -> do
                    liftIO (GH.executeRequest' $
                      GH.commitsWithOptionsForR
                      (GH.N owner) (GH.N repo) (GH.FetchAtLeast 1)
                      [GH.CommitQuerySha branch]) >>= \case
                        Right (toList -> (commit:_)) -> do
                          let GH.N rev = GH.commitSha commit
                          setPackageSpecAttr "rev" (Aeson.String rev)
                        _ -> pure ()
                  _ -> pure ()
                  )
      (_,_) -> pure ()

    -- Figures out the URL template
    whenNotSet "url_template" $
      setPackageSpecAttr
        "url_template"
        (Aeson.String $ T.pack githubURLTemplate)

  where
    githubURLTemplate :: String
    githubURLTemplate =
      "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz"

-------------------------------------------------------------------------------
-- PackageSpec State helpers
-------------------------------------------------------------------------------

whenNotSet
  :: T.Text
  -> StateT PackageSpec IO ()
  -> StateT PackageSpec IO ()
whenNotSet attrName act = getPackageSpecAttr attrName >>= \case
  Just _ -> pure ()
  Nothing -> act

withPackageSpecAttr
  :: T.Text
  -> (Aeson.Value -> StateT PackageSpec IO ())
  -> StateT PackageSpec IO ()
withPackageSpecAttr attrName act = getPackageSpecAttr attrName >>= \case
  Just v -> act v
  Nothing -> pure ()

getPackageSpecAttr
  :: T.Text
  -> StateT PackageSpec IO (Maybe Aeson.Value)
getPackageSpecAttr attrName = do
  PackageSpec obj <- get
  pure $ HMap.lookup attrName obj

setPackageSpecAttr
  :: T.Text -> Aeson.Value
  -> StateT PackageSpec IO ()
setPackageSpecAttr attrName attrValue = do
  PackageSpec obj <- get
  let obj' = HMap.insert attrName attrValue obj
  put (PackageSpec obj')

packageSpecStringValues :: PackageSpec -> [(String, String)]
packageSpecStringValues (PackageSpec m) = mapMaybe toVal (HMap.toList m)
  where
    toVal :: (T.Text, Aeson.Value) -> Maybe (String, String)
    toVal = \case
      (key, Aeson.String val) -> Just (T.unpack key, T.unpack val)
      _ -> Nothing

-------------------------------------------------------------------------------
-- INIT
-------------------------------------------------------------------------------

parseCmdInit :: Opts.ParserInfo (IO ())
parseCmdInit = (Opts.info (pure cmdInit <**> Opts.helper)) Opts.fullDesc

cmdInit :: IO ()
cmdInit = do

    -- Writes all the default files
    forM_
      [ (pathNixVersionsJson, initNixVersionsJsonContent)
      , (pathNixFetchNix, initNixFetchNixContent)
      , (pathNixDefaultNix, initNixDefaultNixContent)
      , (pathDefaultNix, initDefaultNixContent)
      , (pathShellNix, initShellNixContent)
      ] $ \(path, content) -> do
        putStrLn $ "Creating file " <> path <> " (if it doesn't exist)"
        let dir = takeDirectory path
        Dir.createDirectoryIfMissing True dir
        exists <- Dir.doesFileExist path
        if exists
        then do
          putStrLn $ "Not creating " <> path <> " (already exists)"
        else do
          putStrLn $ "Creating " <> path <> " (doesn't exist)"
          writeFile path content

    -- Imports @niv@ and @nixpkgs@ (18.09)
    putStrLn "Importing 'niv' ..."
    cmdAdd (PackageName "nmattia/niv", PackageSpec HMap.empty) Nothing
    putStrLn "Importing 'nixpkgs' ..."
    cmdAdd
      ( PackageName "NixOS/nixpkgs-channels"
      , PackageSpec (HMap.singleton "branch" "nixos-18.09"))
      (Just (PackageName "nixpkgs"))

-------------------------------------------------------------------------------
-- ADD
-------------------------------------------------------------------------------

parseCmdAdd :: Opts.ParserInfo (IO ())
parseCmdAdd =
    Opts.info ((cmdAdd <$> parsePackage <*> optName) <**> Opts.helper)
      Opts.fullDesc
  where
    optName :: Opts.Parser (Maybe PackageName)
    optName = Opts.optional $ PackageName <$>  Opts.strOption
      ( Opts.long "name" <>
        Opts.short 'n' <>
        Opts.metavar "NAME"
      )

cmdAdd :: (PackageName, PackageSpec) -> Maybe PackageName -> IO ()
cmdAdd (PackageName str, spec) mPackageName = do

    -- Figures out the owner and repo
    (packageName, spec') <- flip runStateT spec $ case span (/= '/') str of
          (owner@(_:_), '/':repo@(_:_)) -> do
            whenNotSet "owner" $
              setPackageSpecAttr "owner" (Aeson.String $ T.pack owner)
            whenNotSet "repo" $ do
                setPackageSpecAttr "repo" (Aeson.String $ T.pack repo)
            pure (PackageName repo)
          _ -> pure (PackageName str)

    versionsSpec <- unVersionsSpec <$> getVersionsSpec

    let packageName' = fromMaybe packageName mPackageName

    when (HMap.member packageName' versionsSpec) $ do
      error $ unlines
        [ "Use niv drop <package> and then niv add"
        , "Or use nix update --attr foo bar to update"
        ]

    spec'' <- updatePackageSpec =<< completePackageSpec spec'

    putStrLn $ "Writing new versions file"
    setVersionsSpec $ VersionsSpec $
      HMap.insert packageName' spec'' versionsSpec

-------------------------------------------------------------------------------
-- SHOW
-------------------------------------------------------------------------------

parseCmdShow :: Opts.ParserInfo (IO ())
parseCmdShow = Opts.info (pure cmdShow <**> Opts.helper) Opts.fullDesc

cmdShow :: IO ()
cmdShow = do
    putStrLn $ "Showing versions file"

    fileVersionsValue <- unVersionsSpec <$> getVersionsSpec

    forWithKeyM_ fileVersionsValue $ \key (PackageSpec spec) -> do
      putStrLn $ "Package: " <> unPackageName key
      forM_ (HMap.toList spec) $ \(attrName, attrValValue) -> do
        let attrValue = case attrValValue of
              Aeson.String str -> str
              _ -> "<barabajagal>"
        putStrLn $ "  " <> T.unpack attrName <> ": " <> T.unpack attrValue

-------------------------------------------------------------------------------
-- UPDATE
-------------------------------------------------------------------------------

parseCmdUpdate :: Opts.ParserInfo (IO ())
parseCmdUpdate =
    Opts.info
      ((cmdUpdate <$> Opts.optional parsePackage) <**> Opts.helper)
      Opts.fullDesc

cmdUpdate :: Maybe (PackageName, PackageSpec) -> IO ()
cmdUpdate = \case
    Just (packageName, packageSpec) -> do
      putStrLn $ "Updating single package: " <> unPackageName packageName
      versionsSpec <- unVersionsSpec <$> getVersionsSpec

      packageSpec' <- case HMap.lookup packageName versionsSpec of
        Just packageSpec' -> do
          updatePackageSpec $ PackageSpec $ HMap.union
              (unPackageSpec packageSpec)
              (unPackageSpec packageSpec')
        Nothing -> error $ "Package not found: " <> unPackageName packageName

      setVersionsSpec $ VersionsSpec $
        HMap.insert packageName packageSpec' versionsSpec

    Nothing -> do
      versionsSpec <- unVersionsSpec <$> getVersionsSpec

      versionsSpec' <- forWithKeyM versionsSpec $
        \packageName packageSpec -> do
          putStrLn $ "Package: " <> unPackageName packageName
          updatePackageSpec packageSpec

      setVersionsSpec $ VersionsSpec versionsSpec'

-------------------------------------------------------------------------------
-- DROP
-------------------------------------------------------------------------------

parseCmdDrop :: Opts.ParserInfo (IO ())
parseCmdDrop =
    Opts.info
      ((cmdDrop <$> parsePackageName) <**> Opts.helper)
      Opts.fullDesc

cmdDrop :: PackageName -> IO ()
cmdDrop packageName = do
      putStrLn $ "Dropping package: " <> unPackageName packageName
      versionsSpec <- unVersionsSpec <$> getVersionsSpec

      when (not $ HMap.member packageName versionsSpec) $
        error $ "No such package: " <> unPackageName packageName

      setVersionsSpec $ VersionsSpec $
        HMap.delete packageName versionsSpec

parseCommand :: Opts.Parser (IO ())
parseCommand = Opts.subparser (
    Opts.command "init" parseCmdInit <>
    Opts.command "add"  parseCmdAdd <>
    Opts.command "show"  parseCmdShow <>
    Opts.command "update"  parseCmdUpdate <>
    Opts.command "drop"  parseCmdDrop )

main :: IO ()
main = join $ Opts.execParser opts
  where
    opts = Opts.info (parseCommand <**> Opts.helper)
      ( Opts.fullDesc
     <> Opts.header "NIV - Nix Version manager" )

nixPrefetchURL :: String -> IO String
nixPrefetchURL url =
    lines <$> readProcess "nix-prefetch-url" ["--unpack", url] "" >>=
      \case
        (l:_) -> pure l
        _ -> error "Expected at least one line from nix-prefetch-url"

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
encodeFile fp = L.writeFile fp . Aeson.encode

--- HashMap

forWithKeyM
  :: (Eq k, Hashable k, Monad m)
  => HMap.HashMap k v1
  -> (k -> v1 -> m v2)
  -> m (HMap.HashMap k v2)
forWithKeyM = flip mapWithKeyM

forWithKeyM_
  :: (Eq k, Hashable k, Monad m)
  => HMap.HashMap k v1
  -> (k -> v1 -> m ())
  -> m ()
forWithKeyM_ = flip mapWithKeyM_

mapWithKeyM
  :: (Eq k, Hashable k, Monad m)
  => (k -> v1 -> m v2)
  -> HMap.HashMap k v1
  -> m (HMap.HashMap k v2)
mapWithKeyM f m = do
    fmap mconcat $ forM (HMap.toList m) $ \(k, v) ->
      HMap.singleton k <$> f k v

mapWithKeyM_
  :: (Eq k, Hashable k, Monad m)
  => (k -> v1 -> m ())
  -> HMap.HashMap k v1
  -> m ()
mapWithKeyM_ f m = do
    forM_ (HMap.toList m) $ \(k, v) ->
      HMap.singleton k <$> f k v

-- | Renders the template. Returns 'Nothing' if some of the attributes are
-- missing.
--
--  renderTemplate [("foo", "bar")] "<foo>" == Just "bar"
--  renderTemplate [("foo", "bar")] "<baz>" == Nothing
renderTemplate :: [(String, String)] -> String -> Maybe String
renderTemplate vals = \case
    '<':str -> do
      case span (/= '>') str of
        (key, '>':rest) ->
          liftA2 (<>) (lookup key vals) (renderTemplate vals rest)
        _ -> Nothing
    c:str -> (c:) <$> renderTemplate vals str
    [] -> Just []

-------------------------------------------------------------------------------
-- Files and their content
-------------------------------------------------------------------------------

-- | @nix/fetch.nix@
pathNixFetchNix :: FilePath
pathNixFetchNix = "nix" </> "fetch.nix"

-- | Glue code between nix and versions.json
initNixFetchNixContent :: String
initNixFetchNixContent = [s|
# A record, from name to path, of the third-party packages
let
  versions = builtins.fromJSON (builtins.readFile ./versions.json);
  fetchTarball =
    # fetchTarball version that is compatible between all the versions of
    # Nix
    { url, sha256 }@attrs:
    let
      inherit (builtins) lessThan nixVersion fetchTarball;
    in
      if lessThan nixVersion "1.12" then
        fetchTarball { inherit url; }
      else
        fetchTarball attrs;
in
  builtins.mapAttrs (_: spec:
      fetchTarball {
        url =
          with spec;
          "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
        sha256 = spec.sha256;
      }
    ) versions
|]

-- | @nix/default.nix@
pathNixDefaultNix :: FilePath
pathNixDefaultNix = "nix" </> "default.nix"

-- | File importing @nixpkgs@, setting up overlays, etc
initNixDefaultNixContent :: String
initNixDefaultNixContent = [s|
with { fetch = import ./fetch.nix; };
import fetch.nixpkgs
  { overlays =
      [ (self: super:
          { niv = import fetch.niv {};
          }
        )
      ] ;
    config = { } ;
  }
|]

-- | @default.nix@
pathDefaultNix :: FilePath
pathDefaultNix = "default.nix"

-- | Top level @default.nix@
initDefaultNixContent :: String
initDefaultNixContent = [s|
let pkgs = import ./nix; in pkgs.hello
|]

-- | @shell.nix@
pathShellNix :: FilePath
pathShellNix = "shell.nix"

-- | Simple shell that loads @niv@
initShellNixContent :: String
initShellNixContent = [s|
let pkgs = import ./nix;
in pkgs.mkShell
  { buildInputs = [ pkgs.niv ];
  }
|]

-- | @nix/versions.json"
pathNixVersionsJson :: FilePath
pathNixVersionsJson = "nix" </> "versions.json"

-- | Empty JSON map
initNixVersionsJsonContent :: String
initNixVersionsJsonContent = "{}"
