{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Niv.Git.Cmd where

import Control.Applicative
import Control.Arrow
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
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

gitCmd :: Cmd
gitCmd =
  Cmd
    { description = describeGit,
      parseCmdShortcut = parseGitShortcut,
      parsePackageSpec = parseGitPackageSpec,
      updateCmd = gitUpdate',
      name = "git",
      extraLogs = gitExtraLogs
    }

gitExtraLogs :: Attrs -> [T.Text]
gitExtraLogs attrs = noteRef <> warnRefBranch <> warnRefTag
  where
    noteRef =
      textIf (HMS.member "ref" attrs) $
        mkNote
          "Your source contains a `ref` attribute. Make sure your sources.nix is up-to-date and consider using a `branch` or `tag` attribute."
    warnRefBranch =
      textIf (member "ref" && member "branch") $
        mkWarn
          "Your source contains both a `ref` and a `branch`. Niv will update the `branch` but the `ref` will be used by Nix to fetch the repo."
    warnRefTag =
      textIf (member "ref" && member "tag") $
        mkWarn
          "Your source contains both a `ref` and a `tag`. The `ref` will be used by Nix to fetch the repo."
    member x = HMS.member x attrs
    textIf cond txt = if cond then [txt] else []

parseGitShortcut :: T.Text -> Maybe (PackageName, Aeson.Object)
parseGitShortcut txt'@(T.dropWhileEnd (== '/') -> txt) =
  -- basic heuristics for figuring out if something is a git repo
  if isGitURL
    then case T.splitOn "/" txt of
      [] -> Nothing
      (last -> w) -> case T.stripSuffix ".git" w of
        Nothing -> Just (PackageName w, KM.singleton "repo" (Aeson.String txt'))
        Just w' -> Just (PackageName w', KM.singleton "repo" (Aeson.String txt'))
    else Nothing
  where
    isGitURL =
      ".git" `T.isSuffixOf` txt
        || "git@" `T.isPrefixOf` txt
        || "ssh://" `T.isPrefixOf` txt

parseGitPackageSpec :: Opts.Parser PackageSpec
parseGitPackageSpec =
  (PackageSpec . KM.fromList)
    <$> many (parseRepo <|> parseBranch <|> parseRev <|> parseAttr <|> parseSAttr)
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
    parseBranch =
      ("branch",) . Aeson.String
        <$> Opts.strOption
          ( Opts.long "branch"
              <> Opts.short 'b'
              <> Opts.metavar "BRANCH"
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
      Maybe (K.Key, Aeson.Value)
    parseKeyVal toJSON str = case span (/= '=') str of
      (key, '=' : val) -> Just (K.fromString key, toJSON val)
      _ -> Nothing

describeGit :: Opts.InfoMod a
describeGit =
  mconcat
    [ Opts.fullDesc,
      Opts.progDesc "Add a git dependency. Experimental.",
      Opts.headerDoc $
        Just $
          "Examples:"
            Opts.<$$> ""
            Opts.<$$> "  niv add git git@github.com:stedolan/jq"
            Opts.<$$> "  niv add git ssh://git@github.com/stedolan/jq --rev deadb33f"
            Opts.<$$> "  niv add git https://github.com/stedolan/jq.git"
            Opts.<$$> "  niv add git --repo /my/custom/repo --name custom --branch development"
    ]

gitUpdate ::
  -- | latest rev
  (T.Text -> T.Text -> IO T.Text) ->
  -- | latest rev and default ref
  (T.Text -> IO (T.Text, T.Text)) ->
  Update () ()
gitUpdate latestRev' defaultBranchAndRev' = proc () -> do
  useOrSet "type" -< ("git" :: Box T.Text)
  repository <- load "repo" -< ()
  discoverRev <+> discoverRefAndRev -< repository
  where
    discoverRefAndRev = proc repository -> do
      branchAndRev <- run defaultBranchAndRev' -< repository
      update "branch" -< fst <$> branchAndRev
      update "rev" -< snd <$> branchAndRev
      returnA -< ()
    discoverRev = proc repository -> do
      branch <- load "branch" -< ()
      rev <- run' (uncurry latestRev') -< (,) <$> repository <*> branch
      update "rev" -< rev
      returnA -< ()

-- | The "real" (IO) update
gitUpdate' :: Update () ()
gitUpdate' = gitUpdate latestRev defaultBranchAndRev

latestRev ::
  -- | the repository
  T.Text ->
  -- | the branch
  T.Text ->
  IO T.Text
latestRev repo branch = do
  let gitArgs = ["ls-remote", repo, "refs/heads/" <> branch]
  sout <- runGit gitArgs
  case sout of
    ls@(_ : _ : _) -> abortTooMuchOutput gitArgs ls
    (l1 : []) -> parseRev gitArgs l1
    [] -> abortNoOutput gitArgs
  where
    parseRev args l = maybe (abortNoRev args l) pure $ do
      checkRev $ T.takeWhile (/= '\t') l
    checkRev t = if isRev t then Just t else Nothing
    abortNoOutput args =
      abortGitFailure
        args
        $ "Git didn't produce any output. Does the branch '" <> branch <> "' exist?"
    abortTooMuchOutput args ls =
      abortGitBug args $
        T.unlines $
          ["Git produced too much output:"] <> map ("  " <>) ls

defaultBranchAndRev ::
  -- | the repository
  T.Text ->
  IO (T.Text, T.Text)
defaultBranchAndRev repo = do
  sout <- runGit args
  case sout of
    (l1 : l2 : _) -> (,) <$> parseBranch l1 <*> parseRev l2
    _ ->
      abortGitBug args $
        T.unlines $
          [ "Could not read reference and revision from stdout:"
          ]
            <> sout
  where
    args = ["ls-remote", "--symref", repo, "HEAD"]
    parseBranch l = maybe (abortNoRef args l) pure $ do
      -- ref: refs/head/master\tHEAD -> master\tHEAD
      refAndSym <- T.stripPrefix "ref: refs/heads/" l
      let branch = T.takeWhile (/= '\t') refAndSym
      if T.null branch then Nothing else Just branch
    parseRev l = maybe (abortNoRev args l) pure $ do
      checkRev $ T.takeWhile (/= '\t') l
    checkRev t = if isRev t then Just t else Nothing

abortNoRev :: [T.Text] -> T.Text -> IO a
abortNoRev args l = abortGitBug args $ "Could not read revision from: " <> l

abortNoRef :: [T.Text] -> T.Text -> IO a
abortNoRef args l = abortGitBug args $ "Could not read reference from: " <> l

-- | Run the "git" executable
runGit :: [T.Text] -> IO [T.Text]
runGit args = do
  (exitCode, sout, serr) <- readProcessWithExitCode "git" (T.unpack <$> args) ""
  case (exitCode, lines sout) of
    (ExitSuccess, ls) -> pure $ T.pack <$> ls
    _ ->
      abortGitBug args $
        T.unlines
          [ T.unwords ["stdout:", T.pack sout],
            T.unwords ["stderr:", T.pack serr]
          ]

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

abortGitBug :: [T.Text] -> T.Text -> IO a
abortGitBug args msg =
  abort $
    bug $
      T.unlines
        [ "Could not read the output of 'git'.",
          T.unwords ("command:" : "git" : args),
          msg
        ]
