{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Niv.Logger
  ( job
  , bug
  , tsay
  , say
  , green, tgreen
  , red, tred
  , blue, tblue
  , yellow, tyellow
  , bold, tbold
  , faint, tfaint
  ) where

import Control.Monad
import Data.List
import Data.Profunctor
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import UnliftIO
import qualified System.Console.ANSI as ANSI

type S = String -> String
type T = T.Text -> T.Text

-- XXX: this assumes as single thread
job :: String -> IO () -> IO ()
job str act = do
    say (bold str)
    indent
    tryAny act <* deindent >>= \case
        Right () -> say $ green "Done" <> ": " <> str
        Left e -> do
          -- don't wrap if the error ain't too long
          let showErr = do
                let se = show e
                (if length se > 40 then ":\n" else ": ") <> se
          say $ red "ERROR" <> showErr
          exitFailure
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
    -- we use `intercalate "\n"` because `unlines` prints an extra newline at
    -- the end
    putStrLn $ intercalate "\n" $ (indent <>) <$> lines msg

green :: S
green str =
    ANSI.setSGRCode [ANSI.SetConsoleIntensity ANSI.BoldIntensity] <>
    ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green] <>
    str <> ANSI.setSGRCode [ANSI.Reset]

tgreen :: T
tgreen = t green

yellow :: S
yellow str =
    ANSI.setSGRCode [ANSI.SetConsoleIntensity ANSI.BoldIntensity] <>
    ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow] <>
    str <> ANSI.setSGRCode [ANSI.Reset]

tyellow :: T
tyellow = t yellow

blue :: S
blue str =
    ANSI.setSGRCode [ANSI.SetConsoleIntensity ANSI.BoldIntensity] <>
    ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue] <>
    str <> ANSI.setSGRCode [ANSI.Reset]

tblue :: T
tblue = t blue

red :: S
red str =
    ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red] <>
    str <> ANSI.setSGRCode [ANSI.Reset]

tred :: T
tred = t red

bold :: S
bold str =
    ANSI.setSGRCode [ANSI.SetConsoleIntensity ANSI.BoldIntensity] <>
    ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White] <>
    str <> ANSI.setSGRCode [ANSI.Reset]

tbold :: T
tbold = t bold

faint :: String -> String
faint str =
    ANSI.setSGRCode [ANSI.SetConsoleIntensity ANSI.FaintIntensity] <>
    ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White] <>
    str <> ANSI.setSGRCode [ANSI.Reset]

tfaint :: T
tfaint = t faint

t :: (String -> String) -> T.Text -> T.Text
t = dimap T.unpack T.pack

bug :: T.Text -> T.Text
bug txt = T.unlines
  [ txt
  , "This is a bug. Please create a ticket:"
  , "  https://github.com/nmattia/niv/issues/new"
  , "Thanks! I'll buy you a beer."
  ]
