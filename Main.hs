{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey, (.:))
import Data.Char (toUpper)
import Data.Hashable (Hashable)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.String.QQ (s)
import GHC.Exts (toList)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory)
import System.Process (readProcess)
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified GitHub as GH
import qualified GitHub.Data.Name as GH
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Help.Pretty as Opts
import qualified System.Directory as Dir

main :: IO ()
main = join $ Opts.execParser opts
  where
    opts = Opts.info (parseCommand <**> Opts.helper) $ mconcat desc
    desc =
      [ Opts.fullDesc
      , Opts.header "NIV - Version manager for Nix projects"
      ]

parseCommand :: Opts.Parser (IO ())
parseCommand = Opts.subparser (
    Opts.command "init" parseCmdInit <>
    Opts.command "add"  parseCmdAdd <>
    Opts.command "show"  parseCmdShow <>
    Opts.command "update"  parseCmdUpdate <>
    Opts.command "drop"  parseCmdDrop )

newtype Sources = Sources
  { unSources :: HMap.HashMap PackageName PackageSpec }
  deriving newtype (FromJSON, ToJSON)

getSources :: IO Sources
getSources = do
    -- TODO: if doesn't exist: run niv init
    putStrLn $ "Reading sources file"
    decodeFileStrict pathNixSourcesJson >>= \case
      Just (Aeson.Object obj) ->
        fmap (Sources . mconcat) $
          forM (HMap.toList obj) $ \(k, v) ->
            case v of
              Aeson.Object v' ->
                pure $ HMap.singleton (PackageName (T.unpack k)) (PackageSpec v')
              _ -> abortAttributeIsntAMap
      Just _ -> abortSourcesIsntAMap
      Nothing -> abortSourcesIsntJSON

-- TODO: pretty
setSources :: Sources -> IO ()
setSources sources = encodeFile pathNixSourcesJson sources

newtype PackageName = PackageName { unPackageName :: String }
  deriving newtype (Eq, Hashable, FromJSONKey, ToJSONKey, Show)

parsePackageName :: Opts.Parser PackageName
parsePackageName = PackageName <$>
    Opts.argument Opts.str (Opts.metavar "PACKAGE")

newtype PackageSpec = PackageSpec { _unPackageSpec :: Aeson.Object }
  deriving newtype (FromJSON, ToJSON, Show, Semigroup, Monoid)

parsePackageSpec :: Opts.Parser PackageSpec
parsePackageSpec =
    (PackageSpec . HMap.fromList . fmap fixupAttributes) <$>
      many parseAttribute
  where
    parseAttribute :: Opts.Parser (String, String)
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
    parseKeyVal :: String -> Maybe (String, String)
    parseKeyVal str = case span (/= '=') str of
      (key, '=':val) -> Just (key, val)
      _ -> Nothing

    -- Shortcuts for common attributes
    shortcutAttributes :: Opts.Parser (String, String)
    shortcutAttributes = foldr (<|>) empty $ mkShortcutAttribute <$>
      [ "branch", "owner", "repo", "version" ]

    mkShortcutAttribute :: String -> Opts.Parser (String, String)
    mkShortcutAttribute = \case
      attr@(c:_) -> (attr,) <$> Opts.strOption
        ( Opts.long attr <>
          Opts.short c <>
          Opts.metavar (toUpper <$> attr) <>
          Opts.help
            (
              "Equivalent to --attribute " <>
              attr <> "=<" <> (toUpper <$> attr) <> ">"
            )
        )
      _ -> empty

    fixupAttributes :: (String, String) -> (T.Text, Aeson.Value)
    fixupAttributes (k, v) = (T.pack k, Aeson.String (T.pack v))

parsePackage :: Opts.Parser (PackageName, PackageSpec)
parsePackage = (,) <$> parsePackageName <*> parsePackageSpec

-------------------------------------------------------------------------------
-- PACKAGE SPEC OPS
-------------------------------------------------------------------------------

updatePackageSpec :: PackageSpec -> IO PackageSpec
updatePackageSpec = execStateT $ do
    originalUrl <- getPackageSpecAttr "url"

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

    -- If the type attribute is not set, we try to infer its value based on the url suffix
    (,) <$> getPackageSpecAttr "type" <*> getPackageSpecAttr "url" >>= \case
      -- If an url type is set, we'll use it
      (Just _, _) -> pure ()
      -- We need an url to infer a url type
      (_, Nothing) -> pure ()
      (Nothing, Just (Aeson.String url)) -> do
         let urlType = if "tar.gz" `T.isSuffixOf` url
                       then "tarball"
                       else "file"
         setPackageSpecAttr "type" (Aeson.String $ T.pack urlType)
      -- If the JSON value is not a string, we ignore it
      (_, _) -> pure ()

    -- Updates the sha256 based on the URL contents
    (,) <$> getPackageSpecAttr "url" <*> getPackageSpecAttr "sha256" >>= \case
      -- If no URL is set, we simply can't prefetch
      (Nothing, _) -> pure ()

      -- If an URL is set and no sha is set, /do/ update
      (Just url, Nothing) -> prefetch url

      -- If both the URL and sha are set, update only if the url has changed
      (Just url, Just{}) -> when (Just url /= originalUrl) (prefetch url)

  where
    dockerPrefetch :: StateT PackageSpec IO String
    dockerPrefetch = do
      (,,) <$> getPackageSpecAttr "name" <*> getPackageSpecAttr "tag" <*> getPackageSpecAttr "digest" >>= \case
        (Just (Aeson.String n), Just (Aeson.String t), Just (Aeson.String d)) ->
          liftIO $ (nixPrefetchDockerImage (T.unpack n) (T.unpack t) (T.unpack d))
        _ -> liftIO $ abort "Missing attribute: 'name', 'tag', and 'digest' must be set"

    prefetch :: Aeson.Value -> StateT PackageSpec IO ()
    prefetch = \case
      Aeson.String url -> do
        sha256 <- getPackageSpecAttr "type" >>= \case
          Just (Aeson.String "tarball") -> liftIO $ nixPrefetchURL True (T.unpack url)
          Just (Aeson.String "file")    -> liftIO $ nixPrefetchURL False (T.unpack url)
          Just (Aeson.String "docker")  -> dockerPrefetch
          _ -> liftIO $ abort "Type should be 'tarball', 'file' or 'docker'"
        setPackageSpecAttr "sha256" (Aeson.String $ T.pack sha256)
      _ -> liftIO $ abort "Url must be a string"

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

                whenNotSet "homepage" $ case GH.repoHomepage ghRepo of
                  Just descr ->
                    setPackageSpecAttr "homepage" (Aeson.String descr)
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

    -- If the type is docker, we need to complete the tag and the
    -- digest if they are not specified.
    (,) <$> getPackageSpecAttr "type" <*> getPackageSpecAttr "name" >>= \case
      (Just (Aeson.String "docker"), Just (Aeson.String name)) -> do
         -- If no tag is specified, we consider latest
         whenNotSet "tag" $ setPackageSpecAttr "tag" (Aeson.String (T.pack "latest"))
         whenNotSet "digest" . withPackageSpecAttr "tag" $ \case
           Aeson.String tag -> do
             liftIO (getImageDigest (T.unpack name) (T.unpack tag)) >>= \d -> do
               setPackageSpecAttr "digest" (Aeson.String (T.pack d))
           _ -> pure ()
      (_,_) -> pure ()

    -- Figures out the URL template
    whenNotSet "url_template" $ do
      getPackageSpecAttr "type" >>= \case
        -- The URL template is also used to know if the sha256 needs to be
        -- updated. This is the only reason to create one for the docker fetcher!
        -- Instead of relying on the template_url, it would be better to introduce a function such as
        -- updateNeeded :: PackageSpec -> PackageSpec -> Bool
        -- which takes the old package set, the new one and returns if an update is needed or not
        Just (Aeson.String "docker") -> setPackageSpecAttr
          "url_template"
          (Aeson.String $ T.pack dockerURLTemplate)
        _ -> setPackageSpecAttr
          "url_template"
          (Aeson.String $ T.pack githubURLTemplate)

  where
    githubURLTemplate :: String
    githubURLTemplate =
      "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz"
    dockerURLTemplate :: String
    dockerURLTemplate = "<name>@<digest>"

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
parseCmdInit = Opts.info (pure cmdInit <**> Opts.helper) $ mconcat desc
  where
    desc =
      [ Opts.fullDesc
      , Opts.progDesc
          "Initialize a Nix project. Existing files won't be modified."
      ]

cmdInit :: IO ()
cmdInit = do

    -- Writes all the default files
    forM_
      [ (pathNixSourcesJson, initNixSourcesJsonContent)
      , (pathNixSourcesNix, initNixSourcesNixContent)
      , (pathNixDefaultNix, initNixDefaultNixContent)
      , (pathNixPackagesNix, initNixPackagesNixContent)
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
    cmdAdd Nothing (PackageName "nmattia/niv", PackageSpec HMap.empty)
    putStrLn "Importing 'nixpkgs' ..."
    cmdAdd
      (Just (PackageName "nixpkgs"))
      ( PackageName "NixOS/nixpkgs-channels"
      , PackageSpec (HMap.singleton "branch" "nixos-18.09"))

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
          "  niv add NixOS/nixpkgs-channels -n nixpkgs -b nixos-18.09" Opts.<$$>
          "  niv add my-package -v alpha-0.1 -t http://example.com/archive/<version>.zip"
      ]

cmdAdd :: Maybe PackageName -> (PackageName, PackageSpec) -> IO ()
cmdAdd mPackageName (PackageName str, spec) = do

    -- Figures out the owner and repo
    (packageName, spec') <- flip runStateT spec $ case span (/= '/') str of
          (owner@(_:_), '/':repo@(_:_)) -> do
            whenNotSet "owner" $
              setPackageSpecAttr "owner" (Aeson.String $ T.pack owner)
            whenNotSet "repo" $ do
                setPackageSpecAttr "repo" (Aeson.String $ T.pack repo)
            pure (PackageName repo)
          _ -> pure (PackageName str)

    sources <- unSources <$> getSources

    let packageName' = fromMaybe packageName mPackageName

    when (HMap.member packageName' sources) $
      abortCannotAddPackageExists packageName'

    spec'' <- updatePackageSpec =<< completePackageSpec spec'

    putStrLn $ "Writing new sources file"
    setSources $ Sources $
      HMap.insert packageName' spec'' sources

-------------------------------------------------------------------------------
-- SHOW
-------------------------------------------------------------------------------

parseCmdShow :: Opts.ParserInfo (IO ())
parseCmdShow = Opts.info (pure cmdShow <**> Opts.helper) Opts.fullDesc

cmdShow :: IO ()
cmdShow = do
    putStrLn $ "Showing sources file"

    sources <- unSources <$> getSources

    forWithKeyM_ sources $ \key (PackageSpec spec) -> do
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

cmdUpdate :: Maybe (PackageName, PackageSpec) -> IO ()
cmdUpdate = \case
    Just (packageName, packageSpec) -> do
      putStrLn $ "Updating single package: " <> unPackageName packageName
      sources <- unSources <$> getSources

      packageSpec' <- case HMap.lookup packageName sources of
        Just packageSpec' -> do

          -- TODO: something fishy happening here
          pkgSpec <- completePackageSpec $ packageSpec <> packageSpec'
          updatePackageSpec $ pkgSpec

        Nothing -> abortCannotUpdateNoSuchPackage packageName

      setSources $ Sources $
        HMap.insert packageName packageSpec' sources

    Nothing -> do
      sources <- unSources <$> getSources

      sources' <- forWithKeyM sources $
        \packageName packageSpec -> do
          putStrLn $ "Package: " <> unPackageName packageName
          updatePackageSpec =<< completePackageSpec packageSpec

      setSources $ Sources sources'

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
      putStrLn $ "Dropping package: " <> unPackageName packageName
      sources <- unSources <$> getSources

      when (not $ HMap.member packageName sources) $
        abortCannotDropNoSuchPackage packageName

      setSources $ Sources $
        HMap.delete packageName sources
    attrs -> do
      putStrLn $ "Dropping attributes :" <>
        (T.unpack (T.intercalate " " attrs))
      putStrLn $ "In package: " <> unPackageName packageName
      sources <- unSources <$> getSources

      packageSpec <- case HMap.lookup packageName sources of
        Nothing ->
          abortCannotAttributesDropNoSuchPackage packageName
        Just (PackageSpec packageSpec) -> pure $ PackageSpec $
          HMap.mapMaybeWithKey
            (\k v -> if k `elem` attrs then Nothing else Just v) packageSpec

      setSources $ Sources $
        HMap.insert packageName packageSpec sources

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
encodeFile fp = L.writeFile fp . AesonPretty.encodePretty

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

abort :: String -> IO a
abort msg = do
    putStrLn msg
    exitFailure

nixPrefetchURL :: Bool -> String -> IO String
nixPrefetchURL unpack url =
    lines <$> readProcess "nix-prefetch-url" args "" >>=
      \case
        (l:_) -> pure l
        _ -> abortNixPrefetchExpectedOutput
  where args = if unpack then ["--unpack", url] else [url]

-------------------------------------------------------------------------------
-- Docker image helpers
-------------------------------------------------------------------------------

type ImageName = String
type ImageTag = String
type ImageDigest = String

data SkopeoInspectOutput = SkopeoInspectOutput ImageDigest
instance FromJSON SkopeoInspectOutput where
    parseJSON = Aeson.withObject "SkopeoInspect" $ \v -> SkopeoInspectOutput
        <$> v .: "Digest"

getImageDigest :: ImageName -> ImageTag -> IO ImageDigest
getImageDigest name tag =
    Aeson.decode . TL.encodeUtf8 . TL.pack <$> readProcess "skopeo" [ "inspect", "docker://" ++ name ++ ":" ++ tag ] "" >>=
      \case
        Nothing -> abortSkopeoInspectExpectedOutput
        Just (SkopeoInspectOutput d) -> pure d

-- We use skopeo copy to download the image into a temporary directory
-- from which the image archive is prefetched with nixPrefetchURL
nixPrefetchDockerImage :: ImageName -> ImageTag -> ImageDigest -> IO String
nixPrefetchDockerImage n t d =
    withSystemTempDirectory "niv-skopeo" $ \f -> do
      let src = "docker://" ++ n ++ "@" ++ d
          dstFile =  f ++ "/" ++ (sanitize n)
          dst = "docker-archive://" ++ dstFile ++ ":" ++ n ++ ":" ++ t
      putStrLn $ "Running skopeo copy " ++ src ++ " " ++ dst
      _ <- readProcess "skopeo" [ "copy", src, dst ] ""
      nixPrefetchURL False $ "file://" ++ dstFile
  where
    sanitize = map replace
    replace '/' = '-'
    replace ':' = '-'
    replace c = c

-------------------------------------------------------------------------------
-- Files and their content
-------------------------------------------------------------------------------

-- | @nix/sources.nix@
pathNixSourcesNix :: FilePath
pathNixSourcesNix = "nix" </> "sources.nix"

-- | Glue code between nix and sources.json
initNixSourcesNixContent :: String
initNixSourcesNixContent = [s|
# This file has been generated by Niv.

# A record, from name to path, of the third-party packages
with rec
{
  pkgs = import <nixpkgs> {};

  sources = builtins.fromJSON (builtins.readFile ./sources.json);

  mapAttrs = builtins.mapAttrs or
    (f: set: with builtins;
      listToAttrs (map (attr: { name = attr; value = f attr set.${attr}; }) (attrNames set)));

  callFetcher = spec:
    let fetcherName =
      if builtins.hasAttr "type" spec
      then builtins.getAttr "type" spec
      else "tarball";
    in builtins.getAttr fetcherName {
      "tarball" = pkgs.fetchzip { inherit (spec) url sha256; };
      "file" = pkgs.fetchurl { inherit (spec) url sha256; };
      "docker" = pkgs.dockerTools.pullImage {
        inherit (spec) sha256;
        imageName = spec.name;
        imageDigest = spec.digest;
        finalImageTag = spec.tag;
      };
    };
};
# NOTE: spec must _not_ have an "outPath" attribute
mapAttrs (_: spec:
  if builtins.hasAttr "outPath" spec
  then abort
    "The values in sources.json should not have an 'outPath' attribute"
  else
    if builtins.hasAttr "url" spec && builtins.hasAttr "sha256" spec
    then
      spec //
      { outPath = callFetcher spec; }
    else spec
  ) sources
|]

-- | @nix/default.nix@
pathNixDefaultNix :: FilePath
pathNixDefaultNix = "nix" </> "default.nix"

-- | File importing @nixpkgs@, setting up overlays, etc
initNixDefaultNixContent :: String
initNixDefaultNixContent = [s|
{ sources ? import ./sources.nix }:
with
  { overlay = _: pkgs:
      { inherit (import sources.niv {}) niv;
        packages = pkgs.callPackages ./packages.nix {};
      };
  };
import sources.nixpkgs
  { overlays = [ overlay ] ; config = {}; }
|]

-- | @nix/packages.nix@
pathNixPackagesNix :: FilePath
pathNixPackagesNix = "nix" </> "packages.nix"

-- | File with packages
initNixPackagesNixContent :: String
initNixPackagesNixContent = [s|
{ writeScriptBin
}:
{ foo = writeScriptBin "foo" "echo foo" ; }
|]

-- | @default.nix@
pathDefaultNix :: FilePath
pathDefaultNix = "default.nix"

-- | Top level @default.nix@
initDefaultNixContent :: String
initDefaultNixContent = [s|
let pkgs = import ./nix {}; in pkgs.packages
|]

-- | @shell.nix@
pathShellNix :: FilePath
pathShellNix = "shell.nix"

-- | Simple shell that loads @niv@
initShellNixContent :: String
initShellNixContent = [s|
with { pkgs = import ./nix {}; };
pkgs.mkShell
  { buildInputs = [ pkgs.niv ];
  }
|]

-- | @nix/sources.json"
pathNixSourcesJson :: FilePath
pathNixSourcesJson = "nix" </> "sources.json"

-- | Empty JSON map
initNixSourcesJsonContent :: String
initNixSourcesJsonContent = "{}"

-------------------------------------------------------------------------------
-- Abort
-------------------------------------------------------------------------------

abortSourcesIsntAMap :: IO a
abortSourcesIsntAMap = abort $ unlines [ line1, line2 ]
  where
    line1 = "Cannot use " <> pathNixSourcesJson
    line2 = [s|
The sources file should be a JSON map from package name to package
specification, e.g.:
  { ... }
|]

abortAttributeIsntAMap :: IO a
abortAttributeIsntAMap = abort $ unlines [ line1, line2 ]
  where
    line1 = "Cannot use " <> pathNixSourcesJson
    line2 = [s|
The package specifications in the sources file should be JSON maps from
attribute name to attribute value, e.g.:
  { "nixpkgs": { "foo": "bar" } }
|]

abortSourcesIsntJSON :: IO a
abortSourcesIsntJSON = abort $ unlines [ line1, line2 ]
  where
    line1 = "Cannot use " <> pathNixSourcesJson
    line2 = "The sources file should be JSON."

abortCannotAddPackageExists :: PackageName -> IO a
abortCannotAddPackageExists (PackageName n) = abort $ unlines
    [ "Cannot add package " <> n <> "."
    , "The package already exists. Use"
    , "  nix drop " <> n
    , "and then re-add the package. Alternatively use"
    , "  nix update " <> n <> " --attr foo=bar"
    , "to update the package's attributes."
    ]

abortCannotUpdateNoSuchPackage :: PackageName -> IO a
abortCannotUpdateNoSuchPackage (PackageName n) = abort $ unlines
    [ "Cannot update package " <> n <> "."
    , "The package doesn't exist. Use"
    , "  nix add " <> n
    , "to add the package."
    ]

abortCannotDropNoSuchPackage :: PackageName -> IO a
abortCannotDropNoSuchPackage (PackageName n) = abort $ unlines
    [ "Cannot drop package " <> n <> "."
    , "The package doesn't exist."
    ]

abortCannotAttributesDropNoSuchPackage :: PackageName -> IO a
abortCannotAttributesDropNoSuchPackage (PackageName n) = abort $ unlines
    [ "Cannot drop attributes of package " <> n <> "."
    , "The package doesn't exist."
    ]

abortNixPrefetchExpectedOutput :: IO a
abortNixPrefetchExpectedOutput = abort [s|
Could not read the output of 'nix-prefetch-url'. This is a bug. Please create a
ticket:

  https://github.com/nmattia/niv/issues/new

Thanks! I'll buy you a beer.
|]

abortSkopeoInspectExpectedOutput :: IO a
abortSkopeoInspectExpectedOutput = abort [s|
Could not read the output of 'skopeo inspect'. This is a bug. Please create a
ticket:

  https://github.com/nmattia/niv/issues/new

Thanks! I'll buy you a beer.
|]
