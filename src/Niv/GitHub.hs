{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Niv.GitHub where

import Control.Arrow
import Data.Bool
import Data.Maybe
import qualified Data.Text as T
import Niv.GitHub.API
import Niv.Update

-- | The GitHub update function
-- TODO: fetchers for:
--  * npm
--  * hackage
--  * docker
--  * ... ?
githubUpdate ::
  -- | prefetch
  (Bool -> T.Text -> IO T.Text) ->
  -- | latest revision
  (T.Text -> T.Text -> T.Text -> IO T.Text) ->
  -- | get repo
  (T.Text -> T.Text -> IO GithubRepo) ->
  Update () ()
githubUpdate prefetch latestRev ghRepo = proc () -> do
  urlTemplate <-
    template
      <<< (useOrSet "url_template" <<< completeSpec) <+> (load "url_template")
      -<
        ()
  url <- update "url" -< urlTemplate
  let isTarGuess = (\u -> "tar.gz" `T.isSuffixOf` u || ".tgz" `T.isSuffixOf` u) <$> url
  type' <- useOrSet "type" -< bool "file" "tarball" <$> isTarGuess :: Box T.Text
  let doUnpack = (== "tarball") <$> type'
  _sha256 <- update "sha256" <<< run (\(up, u) -> prefetch up u) -< (,) <$> doUnpack <*> url
  returnA -< ()
  where
    completeSpec :: Update () (Box T.Text)
    completeSpec = proc () -> do
      owner <- load "owner" -< ()
      repo <- load "repo" -< ()
      repoInfo <- run (\(a, b) -> ghRepo a b) -< (,) <$> owner <*> repo
      branch <-
        useOrSet "branch" <<< arr (fmap $ fromMaybe "master")
          -<
            repoDefaultBranch <$> repoInfo
      _description <- useOrSet "description" -< repoDescription <$> repoInfo
      _homepage <- useOrSet "homepage" -< repoHomepage <$> repoInfo
      _ <-
        update "rev" <<< run' (\(a, b, c) -> latestRev a b c)
          -<
            (,,) <$> owner <*> repo <*> branch
      returnA -< pure githubURLTemplate

githubURLTemplate :: T.Text
githubURLTemplate =
  (if githubSecure then "https://" else "http://")
    <> githubHost
    <> githubPath
    <> "<owner>/<repo>/archive/<rev>.tar.gz"
