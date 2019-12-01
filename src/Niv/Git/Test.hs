{-# LANGUAGE OverloadedStrings #-}

module Niv.Git.Test (tests) where

import Niv.Git.Cmd
import Niv.Sources
import Test.Tasty.HUnit ((@=?))
import qualified Data.HashMap.Strict as HMS
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty

tests :: [Tasty.TestTree]
tests = pure $ Tasty.testGroup "repository parse"
  [ Tasty.testCase "goo" $
      parseGitShortcut "goo" @=? Nothing
  , Tasty.testCase "git@github.com:nmattia/niv" $
      parseGitShortcut "git@github.com:nmattia/niv" @=? Just
        (PackageName "niv", HMS.singleton "repo" "git@github.com:nmattia/niv")
  , Tasty.testCase "ssh://git@github.com/stedolan/jq" $
      parseGitShortcut "ssh://git@github.com/stedolan/jq" @=? Just
        (PackageName "jq", HMS.singleton "repo" "ssh://git@github.com/stedolan/jq")
  , Tasty.testCase "https://github.com/stedolan/jq.git" $
      parseGitShortcut "https://github.com/stedolan/jq.git" @=? Just
        (PackageName "jq", HMS.singleton "repo" "https://github.com/stedolan/jq.git")
  , Tasty.testCase "https://github.com/stedolan/jq" $
      parseGitShortcut "https://github.com/stedolan/jq" @=? Nothing
  , Tasty.testCase "~/path/to/repo.git" $
      parseGitShortcut "~/path/to/repo.git" @=? Just
        (PackageName "repo", HMS.singleton "repo" "~/path/to/repo.git")
  ]
