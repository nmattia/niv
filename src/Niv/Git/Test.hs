{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Niv.Git.Test
  ( tests,
  )
where

import Control.Monad
import Data.Bifunctor
import qualified Data.HashMap.Strict as HMS
import Niv.Git.Cmd
import Niv.Sources
import Niv.Update
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit ((@=?))
import qualified Test.Tasty.HUnit as Tasty

tests :: [Tasty.TestTree]
tests = [test_repositoryParse, test_gitUpdates]

test_repositoryParse :: Tasty.TestTree
test_repositoryParse =
  Tasty.testGroup
    "repository parse"
    [ Tasty.testCase "goo" $
        parseGitShortcut "goo" @=? Nothing,
      Tasty.testCase "git@github.com:nmattia/niv" $
        parseGitShortcut "git@github.com:nmattia/niv"
          @=? Just
            (PackageName "niv", HMS.singleton "repo" "git@github.com:nmattia/niv"),
      Tasty.testCase "ssh://git@github.com/stedolan/jq" $
        parseGitShortcut "ssh://git@github.com/stedolan/jq"
          @=? Just
            (PackageName "jq", HMS.singleton "repo" "ssh://git@github.com/stedolan/jq"),
      Tasty.testCase "https://github.com/stedolan/jq.git" $
        parseGitShortcut "https://github.com/stedolan/jq.git"
          @=? Just
            (PackageName "jq", HMS.singleton "repo" "https://github.com/stedolan/jq.git"),
      Tasty.testCase "https://github.com/stedolan/jq" $
        parseGitShortcut "https://github.com/stedolan/jq" @=? Nothing,
      Tasty.testCase "~/path/to/repo.git" $
        parseGitShortcut "~/path/to/repo.git"
          @=? Just
            (PackageName "repo", HMS.singleton "repo" "~/path/to/repo.git")
    ]

test_gitUpdates :: Tasty.TestTree
test_gitUpdates =
  Tasty.testGroup
    "updates"
    [ Tasty.testCase "rev is updated" test_gitUpdateRev
    ]

test_gitUpdateRev :: IO ()
test_gitUpdateRev = do
  interState <- evalUpdate initialState $ proc () ->
    gitUpdate (error "should be def") defaultRefAndHEAD' -< ()
  let interState' = HMS.map (first (\_ -> Free)) interState
  actualState <- evalUpdate interState' $ proc () ->
    gitUpdate latestRev' (error "should update") -< ()
  unless ((snd <$> actualState) == expectedState)
    $ error
    $ "State mismatch: " <> show actualState
  where
    latestRev' _ _ = pure "some-other-rev"
    defaultRefAndHEAD' _ = pure ("some-ref", "some-rev")
    initialState =
      HMS.fromList
        [("repo", (Free, "git@github.com:nmattia/niv"))]
    expectedState =
      HMS.fromList
        [ ("repo", "git@github.com:nmattia/niv"),
          ("ref", "some-ref"),
          ("rev", "some-other-rev"),
          ("type", "git")
        ]
