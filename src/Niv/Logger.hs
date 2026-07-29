{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Niv.Logger
  ( Colors (Always, Never),
    -- job,
    job',
    MyError(SomeError),
    setColors,
    bug,
    tsay,
    tsay',
    say,
    say',
    twarn,
    note',
    warn',
    abort',
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
    Job(..),
  )
where

import Control.Monad
import Control.Monad.Trans (MonadTrans, lift)
import Data.List
import Data.Profunctor
import qualified Data.Text as T
import qualified System.Console.ANSI as ANSI
-- import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO

-- import Control.Concurrent

import           Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import           Control.Monad.Writer (MonadWriter, WriterT, runWriterT, tell)
import qualified Data.Text.IO as T



data MyError
  = SomeError T.Text
  deriving (Show, Eq)

newtype Job io a = Job
  { unJob :: ExceptT MyError (WriterT ([T.Text], [T.Text]) io) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadError MyError
    , MonadWriter ([T.Text], [T.Text])
    , MonadIO
    )

instance MonadTrans Job where
  lift :: Monad io => io a -> Job io a
  lift = Job . lift . lift

abort' :: MonadIO io => T.Text -> Job io a
abort' e = throwError (SomeError e)

-- TODO handle multiline
warn' :: Monad io => T.Text -> Job io ()
warn' w = tell ([], [w])

note' :: Monad io => T.Text -> Job io ()
note' n = tell ([n], [])

-- | Run a Job, getting back the result (or error) plus accumulated log.
job' :: (MonadUnliftIO io, MonadIO io) => T.Text -> Job io a -> io (Either () a)
job' name jb = bracket_ (liftIO ANSI.hideCursor) (liftIO ANSI.showCursor) $ do
    liftIO $ T.putStr fooPending >> T.putStr " "
    hFlush stdout

    (res, (ns, ws)) <- runWriterT . runExceptT . unJob $ jb
    liftIO $ ANSI.setCursorColumn 0

    let foo = case res of
                Left _ -> fooFailure
                Right _ -> 
                    if length ws >= 1 then
                        fooWarning
                        else fooSuccess
    liftIO $ T.putStrLn foo

    forM_ ns $ \w -> printAdmonition (tblue "note") w
    forM_ ws $ \w -> printAdmonition (tyellow "warning") w

    res' <- case res of
        Left (SomeError err) -> printAdmonition (tred "error") err >> pure (Left ())
        Right value -> pure (Right value)
    pure res'
  where
    fooPending = " • " <> tbold name
    fooSuccess = tgreen " ✓ " <> tbold name
    fooWarning = tyellow " ✓ " <> tbold name
    fooFailure = tred " ⨯ " <> tbold name

    printAdmonition admn w = 
        case unsnoc (T.lines w) of
            Nothing -> pure ()
            Just (inits', last') -> do

                liftIO $ T.putStrLn $ "   └ " <> admn <> ": " -- <> w
                forM_ inits' $ \line -> do
                    liftIO $ T.putStrLn $ "     │ " <> line
                liftIO $ T.putStrLn $ "     └ " <> last'

say' :: MonadIO io => String -> Job io ()
say' = tsay' . T.pack

tsay' :: MonadIO io => T.Text -> Job io ()
tsay' msg = do
  let line = "\t" <> msg
  liftIO $ ANSI.clearFromCursorToLineEnd
  liftIO $ T.putStr line
  liftIO $ ANSI.cursorBackward $ T.length line
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

jobStackSize :: (MonadIO io) => io Int
jobStackSize = readIORef jobStack

jobStack :: IORef Int
jobStack = unsafePerformIO $ newIORef 0
{-# NOINLINE jobStack #-}

tsay :: (MonadIO io) => T.Text -> io ()
tsay = say . T.unpack

say :: (MonadIO io) => String -> io ()
say msg = do
  stackSize <- jobStackSize
  let indent = replicate (stackSize * 2) ' '
  -- we use `intercalate "\n"` because `unlines` prints an extra newline at
  -- the end
  liftIO $ putStrLn $ intercalate "\n" $ (indent <>) <$> lines msg

mkWarn :: T.Text -> T.Text
mkWarn w = tbold (tyellow "WARNING") <> ": " <> w

twarn :: (MonadIO io) => T.Text -> io ()
twarn = tsay . mkWarn

mkNote :: T.Text -> T.Text
mkNote w = tbold (tblue "NOTE") <> ": " <> w

color :: ANSI.Color -> String -> String
color c str =
  if useColors
    then
      --ANSI.setSGRCode [ANSI.SetConsoleIntensity ANSI.BoldIntensity] -- TODO: remove bold
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

bug :: T.Text -> T.Text
bug txt =
  T.unlines
    [ txt,
      "This is a bug. Please create a ticket:",
      "  https://github.com/nmattia/niv/issues/new",
      "Thanks! I'll buy you a beer."
    ]
