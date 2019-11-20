{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Niv.Sources where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Bifunctor (first)
import Data.Hashable (Hashable)
import Data.String.QQ (s)
import Niv.GitHub
import Niv.Update
import System.FilePath ((</>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Extended as Aeson
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified System.Directory as Dir

data SourcesError
  = SourcesDoesntExist
  | SourceIsntJSON
  | SpecIsntAMap

newtype Sources = Sources
  { unSources :: HMS.HashMap PackageName PackageSpec }
  deriving newtype (FromJSON, ToJSON)

getSourcesEither :: IO (Either SourcesError Sources)
getSourcesEither = do
    Dir.doesFileExist pathNixSourcesJson >>= \case
      False -> pure $ Left SourcesDoesntExist
      True ->
        Aeson.decodeFileStrict pathNixSourcesJson >>= \case
          Just value -> case valueToSources value of
            Nothing -> pure $ Left SpecIsntAMap
            Just srcs -> pure $ Right srcs
          Nothing -> pure $ Left SourceIsntJSON
  where
    valueToSources :: Aeson.Value -> Maybe Sources
    valueToSources = \case
        Aeson.Object obj -> fmap (Sources . mapKeys PackageName) $ traverse
          (\case
            Aeson.Object obj' -> Just (PackageSpec obj')
            _ -> Nothing
          ) obj
        _ -> Nothing
    mapKeys :: (Eq k2, Hashable k2) => (k1 -> k2) -> HMS.HashMap k1 v -> HMS.HashMap k2 v
    mapKeys f = HMS.fromList . map (first f) . HMS.toList

getSources :: IO Sources
getSources =
    getSourcesEither >>= either
      (\case
        SourcesDoesntExist -> abortSourcesDoesntExist
        SourceIsntJSON -> abortSourcesIsntJSON
        SpecIsntAMap -> abortSpecIsntAMap
      ) pure

setSources :: Sources -> IO ()
setSources sources = Aeson.encodeFilePretty pathNixSourcesJson sources

newtype PackageName = PackageName { unPackageName :: T.Text }
  deriving newtype (Eq, Hashable, FromJSONKey, ToJSONKey, Show)

newtype PackageSpec = PackageSpec { unPackageSpec :: Aeson.Object }
  deriving newtype (FromJSON, ToJSON, Show, Semigroup, Monoid)

-- | Simply discards the 'Freedom'
attrsToSpec :: Attrs -> PackageSpec
attrsToSpec = PackageSpec . fmap snd

-- | @nix/sources.json@
pathNixSourcesJson :: FilePath
pathNixSourcesJson = "nix" </> "sources.json"

-------------------------------------------------------------------------------
-- ABORT messages
-------------------------------------------------------------------------------

abortSourcesDoesntExist :: IO a
abortSourcesDoesntExist = abort $ T.unlines [ line1, line2 ]
  where
    line1 = "Cannot use " <> T.pack pathNixSourcesJson
    line2 = [s|
The sources file does not exist! You may need to run 'niv init'.
|]

abortSourcesIsntJSON :: IO a
abortSourcesIsntJSON = abort $ T.unlines [ line1, line2 ]
  where
    line1 = "Cannot use " <> T.pack pathNixSourcesJson
    line2 = "The sources file should be JSON."

abortSpecIsntAMap :: IO a
abortSpecIsntAMap = abort $ T.unlines [ line1, line2 ]
  where
    line1 = "Cannot use " <> T.pack pathNixSourcesJson
    line2 = [s|
The package specifications in the sources file should be JSON maps from
attribute name to attribute value, e.g.:
  { "nixpkgs": { "foo": "bar" } }
|]
