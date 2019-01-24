{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- TODO: qualified imports
-- TODO: format code

import Control.Monad
import Data.Aeson
import Data.Bifunctor
import Data.Char (toUpper)
import Data.Hashable (Hashable)
import Data.Semigroup ((<>))
import Options.Applicative
import System.Directory
import System.FilePath
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HMap
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

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

-- FOOs

preparePackageURL :: PackageSpec -> IO String
preparePackageURL = const $ pure "foo"

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
parseCmdAdd = (info ((cmdAdd <$> parsePackages) <**> helper)) fullDesc
  where
    parsePackages :: Parser [(PackageName, PackageSpec)]
    parsePackages = some parsePackage

cmdAdd :: [(PackageName, PackageSpec)] -> IO ()
cmdAdd (package@(packageName, _) : _) = do
    putStrLn $ "Adding " <> unPackageName packageName

    print package
    VersionsSpec versionsSpec <- getVersionsSpec

    -- TODO: new package Spec
    let fileVersionsValue' = versionsSpec <> HMap.empty

    putStrLn $ "Writing new versions file"
    encodeFile fileVersionsJson fileVersionsValue'

addCompletePackageSpec
  :: (PackageName, PackageSpec)
  -> IO (PackageName, PackageSpec)
addCompletePackageSpec x = do


    pure x

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
parseCmdUpdate = info (pure cmdUpdate <**> helper) fullDesc

cmdUpdate :: IO ()
cmdUpdate = do
    putStrLn $ "Updating versions file"

    VersionsSpec fileVersionsValue <- getVersionsSpec

    fileVersionsValue' <- forWithKeyM fileVersionsValue $ \key spec -> do
      putStrLn $ "Package: " <> unPackageName key

      packageUrl <- preparePackageURL spec

      putStrLn $ "  URL: " <> packageUrl

      sha256 <- nixPrefetchURL packageUrl

      putStrLn $ " SHA256: " <> sha256

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
