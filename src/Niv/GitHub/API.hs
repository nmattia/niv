{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Niv.GitHub.API where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import Data.Functor
import qualified Data.HashMap.Strict as HMS
import Data.Maybe
import Data.String.QQ (s)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Extended
import qualified Network.HTTP.Simple as HTTP
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

-- Bunch of GitHub helpers

data GithubRepo
  = GithubRepo
      { repoDescription :: Maybe T.Text,
        repoHomepage :: Maybe T.Text,
        repoDefaultBranch :: Maybe T.Text
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
      pure
        GithubRepo
          { repoDescription = lookupText "description",
            repoHomepage = lookupText "homepage",
            repoDefaultBranch = lookupText "default_branch"
          }
    (200, Right v) -> do
      error $ "expected object, got " <> show v
    (200, Left e) -> do
      error $ "github didn't return JSON: " <> show e
    _ -> abortCouldNotFetchGitHubRepo (tshow (request, resp0)) (owner, repo)

-- | TODO: Error instead of T.Text?
abortCouldNotFetchGitHubRepo :: T.Text -> (T.Text, T.Text) -> IO a
abortCouldNotFetchGitHubRepo e (T.unpack -> owner, T.unpack -> repo) = do
  putStrLn $ unlines [line1, line2, T.unpack line3]
  exitFailure
  where
    line1 = "WARNING: Could not read from GitHub repo: " <> owner <> "/" <> repo
    line2 =
      [s|
I assumed that your package was a GitHub repository. An error occurred while
gathering information from the repository. Check whether your package was added
correctly:

  niv show

If not, try re-adding it:

  niv drop <package>
  niv add <package-without-typo>

Make sure the repository exists.
|]
    line3 = T.unwords ["(Error was:", e, ")"]

defaultRequest :: [T.Text] -> IO HTTP.Request
defaultRequest (map T.encodeUtf8 -> parts) = do
  let path = T.encodeUtf8 githubPath <> BS8.intercalate "/" (parts)
  mtoken <- lookupEnv "GITHUB_TOKEN"
  pure
    $ ( flip (maybe id) mtoken $ \token ->
          HTTP.addRequestHeader "authorization" ("token " <> BS8.pack token)
      )
    $ HTTP.setRequestPath path
    $ HTTP.addRequestHeader "user-agent" "niv"
    $ HTTP.addRequestHeader "accept" "application/vnd.github.v3+json"
    $ HTTP.setRequestSecure githubSecure
    $ HTTP.setRequestHost (T.encodeUtf8 githubApiHost)
    $ HTTP.setRequestPort githubApiPort
    $ HTTP.defaultRequest

-- | Get the latest revision for owner, repo and branch.
-- TODO: explain no error handling
githubLatestRev ::
  -- | owner
  T.Text ->
  -- | repo
  T.Text ->
  -- | branch
  T.Text ->
  IO T.Text
githubLatestRev owner repo branch = do
  request <-
    defaultRequest ["repos", owner, repo, "commits", branch]
      <&> HTTP.addRequestHeader "accept" "application/vnd.github.v3.sha"
  resp <- HTTP.httpBS request
  case HTTP.getResponseStatusCode resp of
    200 -> pure $ T.decodeUtf8 $ HTTP.getResponseBody resp
    _ -> abortCouldNotGetRev owner repo branch resp

abortCouldNotGetRev :: T.Text -> T.Text -> T.Text -> HTTP.Response BS8.ByteString -> IO a
abortCouldNotGetRev owner repo branch resp = abort $ T.unlines [line1, line2, line3]
  where
    line1 =
      T.unwords
        [ "Cannot get latest revision for branch",
          "'" <> branch <> "'",
          "(" <> owner <> "/" <> repo <> ")"
        ]
    line2 = "The request failed: " <> tshow resp
    line3 =
      [s|
NOTE: You may want to retry with an authentication token:

    GITHUB_TOKEN=... niv <cmd>

For more information on rate-limiting, see

    https://developer.github.com/v3/#rate-limiting

|]

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
