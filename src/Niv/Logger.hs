{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Niv.Logger where

import qualified System.Console.ANSI as ANSI
import Data.String (IsString)
import UnliftIO

job :: String -> IO () -> IO ()
job str act = do
    say (bold str)
    tryAny act >>= \case
      Right () -> say $ green "Done" <> ": " <> Log str
      Left e -> say $ red "ERROR" <> ":\n" <> Log (show e)

newtype Log = Log { unLog :: String }
  deriving newtype (Semigroup, Monoid, IsString)

say :: Log -> IO ()
say = putStrLn . unLog

green :: String -> Log
green str = Log $
    ANSI.setSGRCode [ANSI.SetConsoleIntensity ANSI.BoldIntensity] <>
    ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green] <>
    str <> ANSI.setSGRCode [ANSI.Reset]

red :: String -> Log
red str = Log $
    ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red] <>
    str <> ANSI.setSGRCode [ANSI.Reset]

bold :: String -> Log
bold str = Log $
    ANSI.setSGRCode [ANSI.SetConsoleIntensity ANSI.BoldIntensity] <>
    ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White] <>
    str <> ANSI.setSGRCode [ANSI.Reset]
