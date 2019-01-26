{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- TODO: qualified imports
-- TODO: format code

import Control.Monad
import Control.Monad.State
import Data.Aeson
import Data.Bifunctor
import Data.Char (toUpper)
import Data.Hashable (Hashable)
import Data.Maybe (mapMaybe)
import Data.Semigroup ((<>))
import Data.String
import GHC.Exts (toList)
import Options.Applicative
import System.Directory
import System.FilePath
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HMap
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified GitHub as GH
import qualified GitHub.Data.Name as GH

fileFetchNix :: FilePath
fileFetchNix = "nix" </> "fetch.nix"

-- TODO: file "nix/default.nix"

fileFetchNixContent :: String
fileFetchNixContent = unlines
  [


  ]

fileVersionsJson :: FilePath
fileVersionsJson = "nix" </> "versions.json"

fileVersionsJsonContent :: String
fileVersionsJsonContent = unlines
  [

  ]

newtype VersionsSpec = VersionsSpec
  { unVersionsSpec :: HMap.HashMap PackageName PackageSpec }
  deriving newtype (FromJSON, ToJSON)

getVersionsSpec :: IO VersionsSpec
getVersionsSpec = do
    putStrLn $ "Reading versions file"
    decodeFileStrict fileVersionsJson >>= \case
      Just (Object v) ->
        fmap (VersionsSpec . mconcat) $
          forM (HMap.toList v) $ \(k, v) ->
            case v of
              Object v' ->
                pure $ HMap.singleton (PackageName (T.unpack k)) (PackageSpec v')
              _ -> error "baaaaz"
      Just _ -> error "foo"
      Nothing -> error "Cannot decode versions"

newtype PackageName = PackageName { unPackageName :: String }
  deriving newtype (Eq, Hashable, FromJSONKey, ToJSONKey, Show)

parsePackageName :: Parser PackageName
parsePackageName = PackageName <$> argument str (metavar "PACKAGE")

newtype PackageSpec = PackageSpec { unPackageSpec :: Object }
  deriving newtype (FromJSON, ToJSON, Show)

parsePackageSpec :: Parser PackageSpec
parsePackageSpec =
    (PackageSpec . HMap.fromList . fmap fixupAttributes) <$>
      many parseAttribute
  where
    parseAttribute :: Parser (String, String)
    parseAttribute = shortcutAttributes <|>
      option (maybeReader parseKeyVal)
        ( long "attribute" <>
          short 'a' <>
          metavar "KEY=VAL"
        )

    -- Parse "key=val" into ("key", "val")
    parseKeyVal :: String -> Maybe (String, String)
    parseKeyVal str = case span (/= '=') str of
      (key, '=':val) -> Just (key, val)
      _ -> Nothing

    -- Shortcuts for common attributes
    shortcutAttributes :: Parser (String, String)
    shortcutAttributes = foldr (<|>) empty $ mkShortcutAttribute <$>
      [ "branch", "name", "owner", "repo" ]

    mkShortcutAttribute :: String -> Parser (String, String)
    mkShortcutAttribute attr@(c:_) = (attr,) <$> strOption
      ( long attr <> short c <> metavar (toUpper <$> attr) )

    fixupAttributes :: (String, String) -> (T.Text, Value)
    fixupAttributes (k, v) = (T.pack k, String (T.pack v))

parsePackage :: Parser (PackageName, PackageSpec)
parsePackage = (,) <$> parsePackageName <*> parsePackageSpec

-------------------------------------------------------------------------------
-- PackageSpec State helpers
-------------------------------------------------------------------------------

whenNotSet
  :: T.Text
  -> StateT (PackageName, PackageSpec) IO ()
  -> StateT (PackageName, PackageSpec) IO ()
whenNotSet attrName act = getPackageSpecAttr attrName >>= \case
  Just _ -> pure ()
  Nothing -> act

withPackageSpecAttr
  :: T.Text
  -> (Value -> StateT (PackageName, PackageSpec) IO ())
  -> StateT (PackageName, PackageSpec) IO ()
withPackageSpecAttr attrName act = getPackageSpecAttr attrName >>= \case
  Just v -> act v
  Nothing -> pure ()

getPackageSpecAttr
  :: T.Text
  -> StateT (PackageName, PackageSpec) IO (Maybe Value)
getPackageSpecAttr attrName = do
  (_, PackageSpec obj) <- get
  pure $ HMap.lookup attrName obj

setPackageSpecAttr
  :: T.Text -> Value
  -> StateT (PackageName, PackageSpec) IO ()
setPackageSpecAttr attrName attrValue = do
  (packageName, PackageSpec obj) <- get
  let obj' = HMap.insert attrName attrValue obj
  put (packageName, PackageSpec obj')

setPackageName
  :: String -> StateT (PackageName, PackageSpec) IO ()
setPackageName packageName = do
  (_, spec) <- get
  put (PackageName packageName, spec)

hasPackageSpecAttrs
  :: [String]
  -> StateT (PackageName, PackageSpec) IO Bool
hasPackageSpecAttrs attrNames = do
  (_, PackageSpec obj) <- get
  pure $ all (\k -> HMap.member (T.pack k) obj) attrNames


packageSpecStringValues :: PackageSpec -> [(String, String)]
packageSpecStringValues (PackageSpec m) = mapMaybe toVal (HMap.toList m)
  where
    toVal :: (T.Text, Value) -> Maybe (String, String)
    toVal = \case
      (key, String val) -> Just (T.unpack key, T.unpack val)
      _ -> Nothing

-------------------------------------------------------------------------------
-- INIT
-------------------------------------------------------------------------------

parseCmdInit :: ParserInfo (IO ())
parseCmdInit = (info (pure cmdInit <**> helper)) fullDesc

cmdInit :: IO ()
cmdInit = do
    putStrLn "Creating directory nix (if it doesn't exist)"
    createDirectoryIfMissing True "nix"

    putStrLn $ "Creating file " <> fileFetchNix <> " (if it doesn't exist)"
    fileFetchNixExists <- doesFileExist fileFetchNix

    if fileFetchNixExists
    then do
      putStrLn $ "Not writing " <> fileFetchNix
      putStrLn "(file exists)"
    else do
      putStrLn $ "Writing " <> fileFetchNix
      writeFile fileFetchNix fileFetchNixContent

    putStrLn $ "Creating file " <> fileVersionsJson <> " (if it doesn't exist)"
    fileVersionsJsonExists <- doesFileExist fileVersionsJson

    if fileVersionsJsonExists
    then do
      putStrLn $ "Not writing " <> fileVersionsJson
      putStrLn "(file exists)"
    else do
      putStrLn $ "Writing " <> fileVersionsJson
      writeFile fileVersionsJson fileVersionsJsonContent

-------------------------------------------------------------------------------
-- ADD
-------------------------------------------------------------------------------

parseCmdAdd :: ParserInfo (IO ())
parseCmdAdd = (info ((cmdAdd <$> parsePackage) <**> helper)) fullDesc

cmdAdd :: (PackageName, PackageSpec) -> IO ()
cmdAdd package = do

    (packageName, packageSpec) <- addCompletePackageSpec package

    versionsSpec <- HMap.insert packageName packageSpec . unVersionsSpec <$>
      getVersionsSpec
    putStrLn $ "Writing new versions file"
    print versionsSpec
    -- encodeFile fileVersionsJson fileVersionsValue'

addCompletePackageSpec
  :: (PackageName, PackageSpec)
  -> IO (PackageName, PackageSpec)
addCompletePackageSpec x@(PackageName str, _) = flip execStateT x $ do

    -- Figures out the owner and repo
    case span (/= '/') str of
          (owner@(_:_), '/':repo@(_:_)) -> do
            whenNotSet "owner" $
              setPackageSpecAttr "owner" (String $ T.pack owner)
            whenNotSet "repo" $ do
                setPackageSpecAttr "repo" (String $ T.pack repo)
                setPackageName repo
          _ -> pure ()

    -- In case we have @owner@ and @repo@, pull some data from GitHub
    (,) <$> getPackageSpecAttr "owner" <*> getPackageSpecAttr "repo" >>= \case
      (Just (String owner), Just (String repo)) -> do
          liftIO (GH.executeRequest' $ GH.repositoryR (GH.N owner) (GH.N repo))
            >>= \case
              Right ghRepo -> do

                -- Description
                whenNotSet "description" $ case GH.repoDescription ghRepo of
                  Just descr -> setPackageSpecAttr "description" (String descr)
                  Nothing -> pure ()

                -- Branch and rev
                whenNotSet "branch" $ case GH.repoDefaultBranch ghRepo of
                  Just branch -> do
                    setPackageSpecAttr "branch" (String branch)
                    liftIO (GH.executeRequest' $
                      GH.commitsWithOptionsForR
                      (GH.N owner) (GH.N repo) (GH.FetchAtLeast 1)
                      [GH.CommitQuerySha branch]) >>= \case
                        Right (toList -> (commit:_)) -> do
                          let GH.N rev = GH.commitSha commit
                          setPackageSpecAttr "rev" (String rev)
                        _ -> pure ()
                  Nothing -> pure ()

    -- Figures out the URL template
    whenNotSet "url_template" $
      setPackageSpecAttr "url_template" (String $ T.pack githubURLTemplate)

    -- Figures out the URL from the template
    withPackageSpecAttr "url_template" (\case
      String (T.unpack -> template) -> do
        (_, packageSpec) <- get
        let stringValues = packageSpecStringValues packageSpec
        case renderTemplate stringValues template of
          Just renderedURL ->
            setPackageSpecAttr "url" (String $ T.pack renderedURL)
          Nothing -> pure ()
      _ -> pure ()
      )
  where
    githubURLTemplate :: String
    githubURLTemplate =
      "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz"

-------------------------------------------------------------------------------
-- SHOW
-------------------------------------------------------------------------------

parseCmdShow :: ParserInfo (IO ())
parseCmdShow = info (pure cmdShow <**> helper) fullDesc

cmdShow :: IO ()
cmdShow = do
    putStrLn $ "Showing versions file"

    VersionsSpec fileVersionsValue <- getVersionsSpec

    forWithKeyM_ fileVersionsValue $ \key (PackageSpec spec) -> do
      putStrLn $ "Package: " <> unPackageName key
      forM_ (HMap.toList spec) $ \(attrName, attrValValue) -> do
        let attrValue = case attrValValue of
              String str -> str
              _ -> "<barabajagal>"
        putStrLn $ "  " <> T.unpack attrName <> ": " <> T.unpack attrValue

-------------------------------------------------------------------------------
-- UPDATE
-------------------------------------------------------------------------------

parseCmdUpdate :: ParserInfo (IO ())
parseCmdUpdate = info ((cmdUpdate <$> parsePackage) <**> helper) fullDesc

cmdUpdate :: (PackageName, PackageSpec) -> IO ()
cmdUpdate pkgs = do
    putStrLn $ "Updating versions file"

    VersionsSpec fileVersionsValue <- getVersionsSpec

    fileVersionsValue' <- forWithKeyM fileVersionsValue $ \key spec -> do
      putStrLn $ "Package: " <> unPackageName key

      -- TODO: use StateT
      -- let packageUrl <- renderTemplate

      -- putStrLn $ "  URL: " <> packageUrl

      -- sha256 <- nixPrefetchURL packageUrl

      -- putStrLn $ " SHA256: " <> sha256

    putStrLn $ "Writing new versions file"
    encodeFile fileVersionsJson fileVersionsValue'

parseCommand :: Parser (IO ())
parseCommand = subparser (
    command "init" parseCmdInit <>
    command "add"  parseCmdAdd <>
    command "show"  parseCmdShow <>
    command "update"  parseCmdUpdate )

main :: IO ()
main = join $ execParser opts
  where
    opts = info (parseCommand <**> helper)
      ( fullDesc
     <> header "NIV - Nix Version manager" )

nixPrefetchURL :: String -> IO String
nixPrefetchURL = pure

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
decodeFileStrict = fmap decodeStrict . B.readFile

-- | Efficiently serialize a JSON value as a lazy 'L.ByteString' and write it to a file.
encodeFile :: (ToJSON a) => FilePath -> a -> IO ()
encodeFile fp = L.writeFile fp . encode

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

    c:str -> (c:) <$> renderTemplate vals str
    [] -> Just []
