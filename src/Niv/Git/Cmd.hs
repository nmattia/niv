{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Niv.Git.Cmd where

import Control.Applicative
import Control.Arrow
import Control.Monad (unless, void)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HMS
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Extended as T
import Niv.Cmd
import Niv.Logger
import Niv.Sources
import Niv.Update
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Help.Pretty as Opts
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)
import UnliftIO

gitCmd :: Cmd
gitCmd =
  Cmd
    { description = describeGit,
      parseCmdShortcut = parseGitShortcut,
      parsePackageSpec = parseGitPackageSpec,
      updateCmd = gitUpdate',
      name = "git"
    }

parseGitShortcut :: T.Text -> Maybe (PackageName, Aeson.Object)
parseGitShortcut txt'@(T.dropWhileEnd (== '/') -> txt) =
  -- basic heuristics for figuring out if something is a git repo
  if isGitURL
    then case T.splitOn "/" txt of
      [] -> Nothing
      (last -> w) -> case T.stripSuffix ".git" w of
        Nothing -> Just (PackageName w, HMS.singleton "repo" (Aeson.String txt'))
        Just w' -> Just (PackageName w', HMS.singleton "repo" (Aeson.String txt'))
    else Nothing
  where
    isGitURL =
      ".git" `T.isSuffixOf` txt
        || "git@" `T.isPrefixOf` txt
        || "ssh://" `T.isPrefixOf` txt

parseGitPackageSpec :: Opts.Parser PackageSpec
parseGitPackageSpec =
  (PackageSpec . HMS.fromList)
    <$> many (parseRepo <|> parseRef <|> parseRev <|> parseAttr <|> parseSAttr)
  where
    parseRepo =
      ("repo",) . Aeson.String
        <$> Opts.strOption
          ( Opts.long "repo"
              <> Opts.metavar "URL"
          )
    parseRev =
      ("rev",) . Aeson.String
        <$> Opts.strOption
          ( Opts.long "rev"
              <> Opts.metavar "SHA"
          )
    parseRef =
      ("ref",) . Aeson.String
        <$> Opts.strOption
          ( Opts.long "ref"
              <> Opts.metavar "REF"
          )
    parseAttr =
      Opts.option
        (Opts.maybeReader parseKeyValJSON)
        ( Opts.long "attribute"
            <> Opts.short 'a'
            <> Opts.metavar "KEY=VAL"
            <> Opts.help "Set the package spec attribute <KEY> to <VAL>, where <VAL> may be JSON."
        )
    parseSAttr =
      Opts.option
        (Opts.maybeReader (parseKeyVal Aeson.toJSON))
        ( Opts.long "string-attribute"
            <> Opts.short 's'
            <> Opts.metavar "KEY=VAL"
            <> Opts.help "Set the package spec attribute <KEY> to <VAL>."
        )
    parseKeyValJSON = parseKeyVal $ \x ->
      fromMaybe (Aeson.toJSON x) (Aeson.decodeStrict (B8.pack x))
    -- Parse "key=val" into ("key", val)
    parseKeyVal ::
      -- | how to convert to JSON
      (String -> Aeson.Value) ->
      String ->
      Maybe (T.Text, Aeson.Value)
    parseKeyVal toJSON str = case span (/= '=') str of
      (key, '=' : val) -> Just (T.pack key, toJSON val)
      _ -> Nothing

describeGit :: Opts.InfoMod a
describeGit =
  mconcat
    [ Opts.fullDesc,
      Opts.progDesc "Add a git dependency. Experimental.",
      Opts.headerDoc $ Just $
        "Examples:"
          Opts.<$$> ""
          Opts.<$$> "  niv add git git@github.com:stedolan/jq"
          Opts.<$$> "  niv add git ssh://git@github.com/stedolan/jq --rev deadb33f"
          Opts.<$$> "  niv add git https://github.com/stedolan/jq.git"
          Opts.<$$> "  niv add git --repo /my/custom/repo --name custom --ref foobar"
    ]

data CommitInfo = CommitInfo {revision :: T.Text, date :: T.Text}

gitUpdate ::
  -- | latest rev
  (T.Text -> T.Text -> IO CommitInfo) ->
  -- | latest rev and default ref
  (T.Text -> IO (T.Text, CommitInfo)) ->
  Update () ()
gitUpdate latestRev' defaultRefAndHEAD' = proc () -> do
  useOrSet "type" -< ("git" :: Box T.Text)
  repository <- load "repo" -< ()
  discoverRev <+> discoverRefAndRev -< repository
  where
    discoverRefAndRev = proc repository -> do
      refAndRev <- run defaultRefAndHEAD' -< repository
      update "ref" -< fst <$> refAndRev
      update "rev" -< (revision . snd) <$> refAndRev
      update "date" -< (date . snd) <$> refAndRev
      returnA -< ()
    discoverRev = proc repository -> do
      ref <- load "ref" -< ()
      rev <- run' (uncurry latestRev') -< (,) <$> repository <*> ref
      update "rev" -< revision <$> rev
      update "date" -< date <$> rev
      returnA -< ()

-- | The "real" (IO) update
gitUpdate' :: Update () ()
gitUpdate' = gitUpdate latestRev defaultRefAndHEAD

-- TODO: document the git operations
latestRevInfo :: T.Text -> Maybe T.Text -> IO (T.Text, CommitInfo)
latestRevInfo repo mref = runGits $ \git -> do
  void $ git ["init"]
  void $ git ["remote", "add", "origin", repo]
  ref <- maybe (git ["remote", "show", "origin"] >>= findRef) pure mref
  void $ git ["fetch", "origin", ref, "--depth", "1"]
  void $ git ["checkout", ref]
  git ["show", "--quiet", "--format=%H%n%aD", ref] >>= \case
    [] -> abort "Git did not produce enough output while reading commit information"
    [rev, dte] -> do
      unless (isRev rev) $ do
        abort $ "The revision retrieved from git does not look like a revision: '" <> rev <> "'."
      pure (ref, CommitInfo {revision = rev, date = dte})
    output ->
      abort $ T.unlines $
        ["Git produced too much output while reading commit information:"] <> output
  where
    findRef ls = case listToMaybe $ mapMaybe (T.stripPrefix "HEAD branch:" . T.strip) ls of
      Just l -> pure (T.strip l)
      Nothing -> abort $ T.unlines $ ["could not parse default ref: "] <> ls

latestRev :: T.Text -> T.Text -> IO CommitInfo
latestRev repo ref = snd <$> latestRevInfo repo (Just ref)

-- TODO: test this
defaultRefAndHEAD ::
  -- | the repository
  T.Text ->
  IO (T.Text, CommitInfo)
defaultRefAndHEAD repo = latestRevInfo repo Nothing

abortNoRev :: [T.Text] -> T.Text -> IO a
abortNoRev args l = abortGitFailure args $ "Could not read revision from: " <> l

abortNoRef :: [T.Text] -> T.Text -> IO a
abortNoRef args l = abortGitFailure args $ "Could not read reference from: " <> l

-- | Run the "git" executable
runGit :: [T.Text] -> IO [T.Text]
runGit args = do
  (exitCode, sout, serr) <- readProcessWithExitCode "git" (T.unpack <$> args) ""
  case (exitCode, lines sout) of
    (ExitSuccess, ls) -> pure $ T.pack <$> ls
    _ ->
      abortGitFailure args $
        T.unlines
          [ T.unwords ["stdout:", T.pack sout],
            T.unwords ["stderr:", T.pack serr]
          ]

runGits :: (([T.Text] -> IO [T.Text]) -> IO a) -> IO a
runGits act = withSystemTempDirectory "niv" $ \f -> do
  past <- newIORef []
  let runGit' args = do
        atomicModifyIORef past (\past' -> (past' <> [args], ()))
        runGit ("-C" : T.pack f : args)
  tryAny (act runGit') >>= \case
    Left e -> do
      past' <- readIORef past
      abort $ bug $ T.unlines $
        [ "An error happened while executing the following git commands in the niv checkout '" <> T.pack f <> "':"
        ]
          <> (map (\cmd -> T.intercalate " " ("  git" : cmd)) past')
          <> [tshow e]
    Right a -> pure a

isRev :: T.Text -> Bool
isRev t =
  -- commit hashes are comprised of abcdef0123456789
  T.all (\c -> (c >= 'a' && c <= 'f') || (c >= '0' && c <= '9')) t
    &&
    -- commit _should_ be 40 chars long, but to be sure we pick 7
    T.length t >= 7

abortGitFailure :: [T.Text] -> T.Text -> IO a
abortGitFailure args msg =
  abort $
    T.unlines
      [ "Could not read the output of 'git'.",
        T.unwords ("command:" : "git" : args),
        msg
      ]
