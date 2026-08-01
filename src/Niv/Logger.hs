{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Niv.Logger
  ( Colors (Always, Never),
    job,
    setColors,
    say,
    note,
    warn,
    throwError,
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
    Job,
  )
where

import Control.Monad
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Writer (MonadWriter, WriterT, runWriterT, tell)
import Data.List
import Data.Profunctor
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Console.ANSI as ANSI
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO

newtype Job io a = Job
  -- A monad stack for use with logging functions below. Can throw a (textual)
  -- error, and gathers admonitions (monad writer: (<notes>, <warnings>))
  { unJob :: ExceptT T.Text (WriterT ([T.Text], [T.Text]) io) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadError T.Text,
      MonadWriter ([T.Text], [T.Text]),
      MonadIO
    )

instance MonadTrans Job where
  lift = Job . lift . lift

warn :: (Monad io) => T.Text -> Job io ()
warn w = tell ([], [w])

note :: (Monad io) => T.Text -> Job io ()
note n = tell ([n], [])

-- | Run a Job, getting back the result (or error) plus accumulated log.
job :: (MonadUnliftIO io, MonadIO io) => T.Text -> Job io a -> io (Either () a)
job name jb = bracket_ (liftIO ANSI.hideCursor) (liftIO ANSI.showCursor) $ do

  -- the "prefixes" that are shown in front of the job name
  let  pending = " • "
       success = tgreen " ✓ "
       warning = tyellow " ✓ "
       failure = tred " ⨯ "

  -- write a "prefix" and the name:
  -- " • foo"
  liftIO $ T.putStr $ pending <> tbold name <> " "
  hFlush stdout

  -- run the job (the 'say' function expects the cursor to be positioned after
  -- the name)
  (res, (ns, ws)) <- runWriterT . runExceptT . unJob $ jb

  -- overwrite the default "prefix" with the result-aware one
  liftIO $ ANSI.setCursorColumn 0
  let prefix = case res of
        Left _ -> failure
        Right _ ->
          if length ws >= 1
            then
              warning
            else success
  liftIO $ T.putStrLn prefix

  -- print admonitions logged during run
  let ns' = fmap (\n -> (tblue "note", n)) ns
  let ws' = fmap (\w -> (tyellow "warning", w)) ws
  -- if the job errored out, add an "error" admonition
  let admns =
        (ns' <> ws') <> case res of
          Left (err) -> [(tred "error", err)]
          Right _ -> []

  printAdmonitions admns

  res' <- case res of
    Left _ -> pure (Left ())
    Right value -> pure (Right value)
  pure res'

-- prints "admonitions":
--
--   ├ note:
--   │ │ this is the first note
--   │ └ which is a multiline note
--   └ note:
--     └ this is another note
printAdmonitions :: (MonadIO io) => [(T.Text, T.Text)] -> io ()
printAdmonitions admns = case unsnoc admns of
  Nothing -> pure ()
  Just (inits', last') -> do
    mapM_ (\(admn, txt) -> printAdmonition admn False txt) inits'
    let (admn, txt) = last'
    printAdmonition admn True txt
  where
    printAdmonition admn isLast txt =
      case unsnoc (T.lines txt) of
        Nothing -> pure ()
        Just (inits', last') -> do
          -- hdr: header (name), idt: indent (alongside text), cls: close (last
          -- line of text).
          -- all are either for the (I)nitial line or the (L)ast line.
          let hdrI = "   ├ " <> admn <> ": "
              hdrL = "   └ " <> admn <> ": "
              idtI line = "   │ │ " <> line
              idtL line = "     │ " <> line
              clsI = "   │ └ " <> last'
              clsL = "     └ " <> last'
              hdr = if isLast then hdrL else hdrI
              idt = if isLast then idtL else idtI
              cls = if isLast then clsL else clsI

          liftIO $ do
            T.putStrLn hdr
            forM_ inits' $ liftIO . T.putStrLn . idt
            T.putStrLn cls

say :: (MonadIO io) => T.Text -> Job io ()
say msg = do
  liftIO $ ANSI.clearFromCursorToLineEnd
  liftIO $ T.putStr msg
  liftIO $ ANSI.cursorBackward $ T.length msg
  hFlush stdout

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
useColors = unsafePerformIO $ (== Always) <$> readIORef colors

type S = String -> String

type T = T.Text -> T.Text

color :: ANSI.Color -> String -> String
color c str =
  if useColors
    then
      ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid c]
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
