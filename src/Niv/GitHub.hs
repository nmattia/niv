{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Niv.GitHub where

import Control.Arrow
import Data.Bool
import Data.Maybe
import Niv.GitHub.API
import Niv.Update
import qualified Data.Text as T

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
    let isTar = (\u -> "tar.gz" `T.isSuffixOf` u || ".tgz" `T.isSuffixOf` u) <$> url
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
