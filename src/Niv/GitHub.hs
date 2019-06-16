{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Niv.GitHub where

import Control.Arrow
import Data.Bool
import Data.Maybe
import Data.String.QQ (s)
import GHC.Exts (toList)
import Niv.Update
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import qualified GitHub as GH
import qualified GitHub.Data.Name as GH
import System.Environment (lookupEnv)

data GithubRepo = GithubRepo
  { repoDescription :: Maybe T.Text
  , repoHomepage :: Maybe T.Text
  , repoDefaultBranch :: Maybe T.Text
  }

githubRepo :: T.Text -> T.Text -> IO GithubRepo
githubRepo owner repo = executeRequest >>= pickResponse >>= return . translate
  where
    pickResponse :: Either GH.Error GH.Repo -> IO GH.Repo
    pickResponse = \case
      Left e -> do
        warnCouldNotFetchGitHubRepo e (owner, repo)
        error (show e)
      Right x -> return x
    resolveRequestExecutionFn = do
      token <- fmap (GH.OAuth . encodeUtf8 . T.pack) <$> lookupEnv "GITHUB_TOKEN"
      return $ maybe GH.executeRequest' GH.executeRequest token
    executeRequest :: IO (Either GH.Error GH.Repo)
    executeRequest = resolveRequestExecutionFn >>= \fn -> fn (GH.repositoryR (GH.N owner) (GH.N repo)) 
    translate :: GH.Repo -> GithubRepo
    translate r = GithubRepo
      { repoDescription = GH.repoDescription r
      , repoHomepage = GH.repoHomepage r
      , repoDefaultBranch = GH.repoDefaultBranch r
      }

warnCouldNotFetchGitHubRepo :: GH.Error -> (T.Text, T.Text) -> IO ()
warnCouldNotFetchGitHubRepo e (T.unpack -> owner, T.unpack -> repo) =
    putStrLn $ unlines [ line1, line2, line3 ]
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
    line3 = unwords [ "(Error was:", show e, ")" ]

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
  "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz"

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
githubLatestRev owner repo branch =
    GH.executeRequest' (
      GH.commitsWithOptionsForR (GH.N owner) (GH.N repo) (GH.FetchAtLeast 1)
      [GH.CommitQuerySha branch]
      ) >>= \case
        Right (toList -> (commit:_)) -> do
          let GH.N rev = GH.commitSha commit
          pure $ rev
        Right (toList -> []) -> do
          error "No rev: no commits"
        Left e -> error $ "No rev: " <> show e
        _ -> error $ "No rev: impossible"
