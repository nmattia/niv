{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Niv.Logger
  ( Colors (Always, Never),
    job,
    setColors,
    bug,
    tsay,
    say,
    twarn,
    mkWarn,
    mkNote,
    green,
    tgreen,
    red,
    tred,
    blue,
    tblue,
    yellow,
    tyellow,
    bold,
    tbold,
    faint,
    tfaint,
  )
where

import Control.Monad
import Data.List
import Data.Profunctor
import qualified Data.Text as T
import qualified System.Console.ANSI as ANSI
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO

-- A somewhat hacky way of deciding whether or not to use SGR codes, by writing
-- and reading a global variable unsafely.
-- This should be fine as long as the IORef is written right after argument
-- parsing, and as long as the value is never changed.
-- NOTE: this won't work in GHCi.

data Colors
  = Always
  | Never
  deriving (Eq)

colors :: IORef Colors
colors = unsafePerformIO $ newIORef Always
{-# NOINLINE colors #-}

setColors :: Colors -> IO ()
setColors = writeIORef colors

useColors :: Bool
useColors = unsafePerformIO $ (\c -> c == Always) <$> readIORef colors

type S = String -> String

type T = T.Text -> T.Text

-- XXX: this assumes as single thread
job :: (MonadUnliftIO io, MonadIO io) => String -> io () -> io ()
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
      liftIO exitFailure
  where
    indent = void $ atomicModifyIORef jobStack (\x -> (x + 1, undefined))
    deindent = void $ atomicModifyIORef jobStack (\x -> (x - 1, undefined))

jobStackSize :: MonadIO io => io Int
jobStackSize = readIORef jobStack

jobStack :: IORef Int
jobStack = unsafePerformIO $ newIORef 0

{-# NOINLINE jobStackSize #-}

tsay :: MonadIO io => T.Text -> io ()
tsay = say . T.unpack

say :: MonadIO io => String -> io ()
say msg = do
  stackSize <- jobStackSize
  let indent = replicate (stackSize * 2) ' '
  -- we use `intercalate "\n"` because `unlines` prints an extra newline at
  -- the end
  liftIO $ putStrLn $ intercalate "\n" $ (indent <>) <$> lines msg

mkWarn :: T.Text -> T.Text
mkWarn w = tbold (tyellow "WARNING") <> ": " <> w

twarn :: MonadIO io => T.Text -> io ()
twarn = tsay . mkWarn

mkNote :: T.Text -> T.Text
mkNote w = tbold (tblue "NOTE") <> ": " <> w

color :: ANSI.Color -> String -> String
color c str =
  if useColors
    then
      ANSI.setSGRCode [ANSI.SetConsoleIntensity ANSI.BoldIntensity]
        <> ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid c]
        <> str
        <> ANSI.setSGRCode [ANSI.Reset]
    else str

colorFaint :: ANSI.Color -> String -> String
colorFaint c str =
  if useColors
    then
      ANSI.setSGRCode [ANSI.SetConsoleIntensity ANSI.FaintIntensity]
        <> ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid c]
        <> str
        <> ANSI.setSGRCode [ANSI.Reset]
    else str

green :: S
green = color ANSI.Green

tgreen :: T
tgreen = t green

yellow :: S
yellow = color ANSI.Yellow

tyellow :: T
tyellow = t yellow

blue :: S
blue = color ANSI.Blue

tblue :: T
tblue = t blue

red :: S
red = color ANSI.Red

tred :: T
tred = t red

bold :: S
bold = color ANSI.White

tbold :: T
tbold = t bold

faint :: String -> String
faint = colorFaint ANSI.White

tfaint :: T
tfaint = t faint

t :: (String -> String) -> T.Text -> T.Text
t = dimap T.unpack T.pack

bug :: T.Text -> T.Text
bug txt =
  T.unlines
    [ txt,
      "This is a bug. Please create a ticket:",
      "  https://github.com/nmattia/niv/issues/new",
      "Thanks! I'll buy you a beer."
    ]
