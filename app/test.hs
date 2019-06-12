module Main where

import Niv.Test
import qualified Test.Tasty as Tasty

main :: IO ()
main = Tasty.defaultMain $ Niv.Test.tests

