{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Niv.GitHub where

import Control.Arrow
import Data.Bool
import Data.Functor
import Data.Maybe
import Data.String.QQ (s)
import Niv.Update
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Network.HTTP.Simple as HTTP

-- | The GitHub update function
-- TODO: fetchers for:
--  * npm
--  * hackage
--  * docker
--  * ... ?
githubUpdate
  :: (Bool -> T.Text -> IO T.Text)
  -- ^ prefetch
  -> (T.Text -> T.Text -> T.Text -> IO T.Text)
  -- ^ latest revision
  -> (T.Text -> T.Text -> IO GithubRepo)
  -- ^ get repo
  -> Update () ()
githubUpdate prefetch latestRev ghRepo = proc () -> do
    urlTemplate <- template <<<
      (useOrSet "url_template" <<< completeSpec) <+> (load "url_template") -<
      ()
    url <- update "url" -< urlTemplate
    let isTar = ("tar.gz" `T.isSuffixOf`) <$> url
    useOrSet "type" -< bool "file" "tarball" <$> isTar :: Box T.Text
    let doUnpack = isTar
    _sha256 <- update "sha256" <<< run (\(up, u) -> prefetch up u) -< (,) <$> doUnpack <*> url
    returnA -< ()
  where
    completeSpec :: Update () (Box T.Text)
    completeSpec = proc () -> do
      owner <- load "owner" -< ()
      repo <- load "repo" -< ()
      repoInfo <- run (\(a, b) -> ghRepo a b) -< (,) <$> owner <*> repo
      branch <- useOrSet "branch" <<< arr (fmap $ fromMaybe "master") -<
        repoDefaultBranch <$> repoInfo
      _description <- useOrSet "description" -< repoDescription <$> repoInfo
      _homepage <- useOrSet "homepage" -< repoHomepage <$> repoInfo
      _ <- update "rev" <<< run' (\(a,b,c) -> latestRev a b c) -<
        (,,) <$> owner <*> repo <*> branch
      returnA -< pure githubURLTemplate

githubURLTemplate :: T.Text
githubURLTemplate =
  (if githubSecure then "https://" else "http://") <>
  githubHost <> githubPath <> "<owner>/<repo>/archive/<rev>.tar.gz"

-- Bunch of GitHub helpers

data GithubRepo = GithubRepo
  { repoDescription :: Maybe T.Text
  , repoHomepage :: Maybe T.Text
  , repoDefaultBranch :: Maybe T.Text
  }

githubRepo :: T.Text -> T.Text -> IO GithubRepo
githubRepo owner repo = do
    request <- defaultRequest ["repos", owner, repo]
    -- we don't use httpJSONEither because it adds an "Accept:
    -- application/json" header that GitHub chokes on
    resp0 <- HTTP.httpBS request
    let resp = fmap Aeson.eitherDecodeStrict resp0
    case (HTTP.getResponseStatusCode resp, HTTP.getResponseBody resp) of
      (200, Right (Aeson.Object m)) -> do
        let lookupText k = case HMS.lookup k m of
              Just (Aeson.String t) -> Just t
              _ -> Nothing
        pure GithubRepo
          { repoDescription = lookupText "description"
          , repoHomepage = lookupText "homepage"
          , repoDefaultBranch = lookupText "default_branch"
          }
      (200, Right v) -> do
        error $ "expected object, got " <> show v
      (200, Left e) -> do
        error $ "github didn't return JSON: " <> show e
      _ -> abortCouldNotFetchGitHubRepo (tshow (request,resp0)) (owner, repo)

-- | TODO: Error instead of T.Text?
abortCouldNotFetchGitHubRepo :: T.Text -> (T.Text, T.Text) -> IO a
abortCouldNotFetchGitHubRepo e (T.unpack -> owner, T.unpack -> repo) = do
    putStrLn $ unlines [ line1, line2, T.unpack line3 ]
    exitFailure
  where
    line1 = "WARNING: Could not read from GitHub repo: " <> owner <> "/" <> repo
    line2 = [s|
I assumed that your package was a GitHub repository. An error occurred while
gathering information from the repository. Check whether your package was added
correctly:

  niv show

If not, try re-adding it:

  niv drop <package>
  niv add <package-without-typo>

Make sure the repository exists.
|]
    line3 = T.unwords [ "(Error was:", e, ")" ]

defaultRequest :: [T.Text] -> IO HTTP.Request
defaultRequest (map T.encodeUtf8 -> parts) = do
    let path = T.encodeUtf8 githubPath <> BS8.intercalate "/" (parts)
    mtoken <- lookupEnv "GITHUB_TOKEN"
    pure $
      (flip (maybe id) mtoken $ \token ->
        HTTP.addRequestHeader "authorization" ("token " <> BS8.pack token)
      ) $
      HTTP.setRequestPath path $
      HTTP.addRequestHeader "user-agent" "niv" $
      HTTP.addRequestHeader "accept" "application/vnd.github.v3+json" $
      HTTP.setRequestSecure githubSecure $
      HTTP.setRequestHost (T.encodeUtf8 githubApiHost) $
      HTTP.setRequestPort githubApiPort $
      HTTP.defaultRequest

-- | Get the latest revision for owner, repo and branch.
-- TODO: explain no error handling
githubLatestRev
  :: T.Text
  -- ^ owner
  -> T.Text
  -- ^ repo
  -> T.Text
  -- ^ branch
  -> IO T.Text
githubLatestRev owner repo branch = do
    request <- defaultRequest [ "repos", owner, repo, "commits", branch ] <&>
          HTTP.addRequestHeader "accept" "application/vnd.github.v3.sha"
    resp <- HTTP.httpBS request
    case HTTP.getResponseStatusCode resp of
      200 -> pure $ T.decodeUtf8 $ HTTP.getResponseBody resp
      _ -> abortCouldNotGetRev owner repo branch resp

abortCouldNotGetRev :: T.Text -> T.Text -> T.Text -> HTTP.Response BS8.ByteString -> IO a
abortCouldNotGetRev owner repo branch resp = abort $ T.unlines [ line1, line2, line3 ]
  where
    line1 = T.unwords
      [ "Cannot get latest revision for branch"
      , "'" <> branch <> "'"
      , "(" <> owner <> "/" <> repo <> ")"
      ]
    line2 = "The request failed: " <> tshow resp
    line3 =  [s|
NOTE: You may want to retry with an authentication token:

    GITHUB_TOKEN=... niv <cmd>

For more information on rate-limiting, see

    https://developer.github.com/v3/#rate-limiting

|]

-- TODO: document all of these

githubHost :: T.Text
githubHost = unsafePerformIO $ do
    lookupEnv "GITHUB_HOST" >>= \case
      Just (T.pack -> x) -> pure x
      Nothing -> pure "github.com"

githubApiPort :: Int
githubApiPort = unsafePerformIO $ do
    lookupEnv "GITHUB_API_PORT" >>= \case
      Just (readMaybe -> Just x) -> pure x
      _ -> pure $ if githubSecure then 443 else 80

githubApiHost :: T.Text
githubApiHost = unsafePerformIO $ do
    lookupEnv "GITHUB_API_HOST" >>= \case
      Just (T.pack -> x) -> pure x
      Nothing -> pure "api.github.com"

githubSecure :: Bool
githubSecure = unsafePerformIO $ do
    lookupEnv "GITHUB_INSECURE" >>= \case
      Just "" -> pure True
      Just _ -> pure False
      Nothing -> pure True

githubPath :: T.Text
githubPath = unsafePerformIO $ do
    lookupEnv "GITHUB_PATH" >>= \case
      Just (T.pack -> x) -> pure $ fromMaybe x (T.stripSuffix "/" x) <> "/"
      Nothing -> pure "/"

abort :: T.Text -> IO a
abort msg = do
    T.putStrLn msg
    exitFailure

tshow :: Show a => a -> T.Text
tshow = T.pack . show
