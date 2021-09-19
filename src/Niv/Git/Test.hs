{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Niv.Git.Test
  ( tests,
  )
where

import Control.Monad
import Data.Bifunctor
import qualified Data.HashMap.Strict as HMS
import Data.IORef
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
    [ Tasty.testCase "rev is updated" test_gitUpdateRev,
      Tasty.testCase "git is called once" test_gitCalledOnce
    ]

test_gitUpdateRev :: IO ()
test_gitUpdateRev = do
  interState <-
    evalUpdate (PackageName "Test") initialState $
      gitUpdate (error "should be def") defaultBranchAndHEAD'
  let interState' = HMS.map (first (\_ -> Free)) interState
  actualState <-
    evalUpdate (PackageName "Test") interState' $
      gitUpdate latestRev' (error "should update")
  unless ((snd <$> actualState) == expectedState) $
    error $
      "State mismatch: " <> show actualState
  where
    latestRev' _ _ = pure "some-other-rev"
    defaultBranchAndHEAD' _ = pure ("some-branch", "some-rev")
    initialState =
      HMS.fromList
        [("repo", (Free, "git@github.com:nmattia/niv"))]
    expectedState =
      HMS.fromList
        [ ("repo", "git@github.com:nmattia/niv"),
          ("branch", "some-branch"),
          ("rev", "some-other-rev"),
          ("type", "git")
        ]

once1 :: (b -> IO a) -> IO (b -> IO a)
once1 f = do
  used <- newIORef False
  pure $ \x -> do
    used' <- readIORef used
    if used'
      then error "already used"
      else do
        writeIORef used True
        f x

once2 :: (a -> b -> IO c) -> IO (a -> b -> IO c)
once2 f = do
  used <- newIORef False
  pure $ \x y -> do
    used' <- readIORef used
    if used'
      then error "already used"
      else do
        writeIORef used True
        f x y

-- | This tests that we don't run the same git operations several times during
-- the update
test_gitCalledOnce :: IO ()
test_gitCalledOnce = do
  defaultBranchAndHEAD'' <- once1 defaultBranchAndHEAD'
  latestRev'' <- once2 latestRev'
  interState <-
    evalUpdate (PackageName "Test") initialState $
      gitUpdate (error "should be def") defaultBranchAndHEAD''
  let interState' = HMS.map (first (\_ -> Free)) interState
  actualState <-
    evalUpdate (PackageName "Test") interState' $
      gitUpdate latestRev'' (error "should update")
  unless ((snd <$> actualState) == expectedState) $
    error $
      "State mismatch: " <> show actualState
  where
    latestRev' _ _ = pure "some-other-rev"
    defaultBranchAndHEAD' _ = pure ("some-branch", "some-rev")
    initialState =
      HMS.fromList
        [("repo", (Free, "git@github.com:nmattia/niv"))]
    expectedState =
      HMS.fromList
        [ ("repo", "git@github.com:nmattia/niv"),
          ("branch", "some-branch"),
          ("rev", "some-other-rev"),
          ("type", "git")
        ]
