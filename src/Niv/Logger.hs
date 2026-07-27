{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Niv.Logger
  ( Colors (Always, Never),
    job,
    job',
    setColors,
    bug,
    tsay,
    tsay',
    say,
    say',
    twarn,
    note',
    warn',
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
import System.Exit (exitFailure)
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

abort' :: MonadIO io => T.Text -> Job io ()
abort' e = throwError (SomeError e)

-- TODO handle multiline
warn' :: Monad io => T.Text -> Job io ()
warn' w = tell ([], [w])

-- TODO handle multiline
note' :: Monad io => T.Text -> Job io ()
note' n = tell ([n], [])

-- | Run a Job, getting back the result (or error) plus accumulated log.
job' :: (MonadUnliftIO io, MonadIO io) => T.Text -> Job io a -> io (Either MyError a, [T.Text])
job' name jb = bracket_ (liftIO ANSI.hideCursor) (liftIO ANSI.showCursor) $ do
    liftIO $ T.putStr fooPending >> T.putStr " "
    hFlush stdout

    (res, (ns, ws)) <- runWriterT . runExceptT . unJob $ jb
    liftIO $ ANSI.setCursorColumn 0
    let foo = case res of
                Left _ -> fooFailure
                Right _ -> fooSuccess
    liftIO $ T.putStrLn foo

    forM_ ns $ \w ->
        liftIO $ T.putStrLn $ "   ∟ " <> tblue "note" <> ": " <> w

    forM_ ws $ \w ->
        liftIO $ T.putStrLn $ "   ∟ " <> tyellow "warning" <> ": " <> w

    case res of
        Left (SomeError err) ->
            liftIO $ T.putStrLn $ "   ∟ " <> tred "error" <> ": " <> err
        Right _ -> pure ()
    pure (res, ws)
  where
    fooPending = " • " <> tbold name
    fooSuccess = tgreen " ✓ " <> tbold name
    fooFailure = tred " ⨯ " <> tbold name

say' :: MonadIO io => String -> io ()
say' = tsay' . T.pack

tsay' :: MonadIO io => T.Text -> io ()
tsay' msg = do
  liftIO $ ANSI.clearFromCursorToLineEnd
  liftIO $ T.putStr $ "\t" <> msg
  liftIO $ ANSI.cursorBackward $ T.length msg
  hFlush stdout


-- test :: IO ()
-- test = do
--     let job1 = do
--             blurt "Hei"
--             liftIO $ threadDelay  1000000
--             blurt "HA"
--             liftIO $ ANSI.setCursorColumn 0
--             hFlush stdout
--             liftIO $ threadDelay  1000000
--             warnFOO "what is happening"
--             abort "Ok, no more"
--
--     let job2 = do
--             blurt "starting..."
--             liftIO $ threadDelay  1000000
--             blurt "updating..."
--             liftIO $ threadDelay  1000000
--             blurt "done!"
--
--     forM_ [("one", job1), ("two", job2)] $ \(nm,jb) -> runJob nm jb
--
-- testGood :: IO ()
-- testGood = do
--     let job1 = do
--             blurt "success"
--
--     let job2 = do
--             blurt "success"
--
--     forM_ [("one", job1), ("two", job2)] $ \(nm,jb) -> runJob nm jb

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

-- XXX: this assumes as single thread
job :: (MonadUnliftIO io, MonadIO io) => String -> io a -> io a
job str act = do
  say (bold str)
  indent
  tryAny act <* deindent >>= \case
    Right result -> do
        say $ green "Done" <> ": " <> str
        pure result
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
      ANSI.setSGRCode [ANSI.SetConsoleIntensity ANSI.BoldIntensity] -- TODO: remove bold
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
