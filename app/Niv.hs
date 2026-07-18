module Main where

import Niv.Cli
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= Niv.Cli.cli
