{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Niv.Logger where

import Control.Monad
import Data.Profunctor
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import UnliftIO
import qualified System.Console.ANSI as ANSI

-- XXX: this assumes as single thread
job :: String -> IO () -> IO ()
job str act = do
    say (bold str)
    indent
    tryAny act <* deindent >>= \case
        Right () -> say $ green "Done" <> ": " <> str
        Left e -> say $ red "ERROR" <> ":\n" <> show e
  where
    indent = void $ atomicModifyIORef jobStack (\x -> (x + 1, undefined))
    deindent = void $ atomicModifyIORef jobStack (\x -> (x - 1, undefined))

jobStackSize :: IO Int
jobStackSize = readIORef jobStack

jobStack :: IORef Int
jobStack = unsafePerformIO $ newIORef 0
{-# NOINLINE jobStackSize #-}

tsay :: T.Text -> IO ()
tsay = say . T.unpack

say :: String -> IO ()
say msg = do
    stackSize <- jobStackSize
    let indent = replicate (stackSize * 2) ' '
    putStrLn $ indent <> msg

green :: String -> String
green str =
    ANSI.setSGRCode [ANSI.SetConsoleIntensity ANSI.BoldIntensity] <>
    ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green] <>
    str <> ANSI.setSGRCode [ANSI.Reset]

red :: String -> String
red str =
    ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red] <>
    str <> ANSI.setSGRCode [ANSI.Reset]

tbold :: T.Text -> T.Text
tbold = dimap T.unpack T.pack bold

bold :: String -> String
bold str =
    ANSI.setSGRCode [ANSI.SetConsoleIntensity ANSI.BoldIntensity] <>
    ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White] <>
    str <> ANSI.setSGRCode [ANSI.Reset]

tfaint :: T.Text -> T.Text
tfaint = dimap T.unpack T.pack faint

faint :: String -> String
faint str =
    ANSI.setSGRCode [ANSI.SetConsoleIntensity ANSI.FaintIntensity] <>
    ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White] <>
    str <> ANSI.setSGRCode [ANSI.Reset]
