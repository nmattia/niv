{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Niv.Custom.Test (tests) where

import Control.Monad
import Data.Bifunctor
import Niv.Custom.Cmd
import Niv.Sources
import Niv.Update
import Test.Tasty.HUnit ((@=?))
import qualified Data.HashMap.Strict as HMS
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty

tests :: [Tasty.TestTree]
tests = [ test_foo ]

test_foo :: Tasty.TestTree
test_foo = Tasty.testGroup "bar"
  [ Tasty.testCase "baz" $ pure ()
  ]
