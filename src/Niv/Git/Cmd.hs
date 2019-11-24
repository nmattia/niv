{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE QuasiQuotes #-}

module Niv.Git.Cmd {- (gitCmd) -} where

import Control.Arrow
import Data.String.QQ (s)
import Data.Text.Extended as T
import Niv.Cmd
import Niv.Sources
import Niv.Update
import System.Exit (ExitCode(ExitSuccess))
import System.Process (readProcessWithExitCode)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Help.Pretty as Opts

gitCmd :: Cmd
gitCmd = Cmd
  { description = describeGit
  , parseCmdShortcut = pure Nothing
  , parsePackageSpec = parseGitPackageSpec
  , updateCmd = gitUpdate
  , name = "git"
  }

parseGitPackageSpec :: Opts.Parser PackageSpec
parseGitPackageSpec = pure $ PackageSpec $ HMS.singleton "repo" "git@github.com:nmattia/niv"

describeGit :: Opts.InfoMod a
describeGit = mconcat
  [ Opts.fullDesc
  , Opts.progDesc "Add a git dependency. Experimental."
  , Opts.headerDoc $ Just $
      "Examples:" Opts.<$$>
      "" Opts.<$$>
      "  niv add git@github.com:stedolan/jq" Opts.<$$>
      "  niv add ssh://git@github.com/stedolan/jq" Opts.<$$>
      "  niv add https://github.com/stedolan/jq.git"
  ]

gitUpdate :: Update () ()
gitUpdate = proc () -> do
    useOrSet "type" -< ("git" :: Box T.Text)
    repository <- load "repo" -< ()
    refAndRev <- (discoverRev <+> discoverRefAndRev) -< repository
    update "ref" -< fst <$> refAndRev
    update "rev" -< snd <$> refAndRev
    returnA -< ()
  where
    discoverRefAndRev = proc repository -> do
      run defaultRefAndHEAD -< repository
    discoverRev = proc repository -> do
      ref <- load "ref" -< ()
      rev <- run (\(r1,r2) -> latestRev r1 r2)-< (,) <$> repository <*> ref
      returnA -< (,) <$> ref <*> rev

latestRev
    :: T.Text -- ^ the repository
    -> T.Text -- ^ the ref/branch
    -> IO T.Text
latestRev repo ref = do
    let gitArgs = [ "ls-remote", repo, "refs/heads/" <> ref ]
    sout <- runGit gitArgs
    case sout of
      ls@(_:_:_) -> abortTooMuchOutput ls
      (l1:[]) -> parseRev l1
      [] -> abortNoOutput
  where
    parseRev l = maybe (abortNoRev l) pure $ do
      checkRev $ T.takeWhile (/= '\t') l
    checkRev t = if isRev t then Just t else Nothing
    abortNoOutput = abort "foo" -- TODO: args + abortBugIn

defaultRefAndHEAD
    :: T.Text -- ^ the repository
    -> IO (T.Text, T.Text)
defaultRefAndHEAD repo = do
    sout <- runGit [ "ls-remote", "--symref", repo, "HEAD" ]
    case sout of
      (l1:l2:_) -> (,) <$> parseRef l1 <*> parseRev l2
      _ -> abortNoRefAndRev
  where
    parseRef l = maybe (abortNoRef l) pure $ do
      -- ref: refs/head/master\tHEAD -> master\tHEAD
      refAndSym <- T.stripPrefix "ref: refs/heads/" l
      let ref = T.takeWhile (/= '\t') refAndSym
      if T.null ref then Nothing else Just ref
    parseRev l = maybe (abortNoRev l) pure $ do
      checkRev $ T.takeWhile (/= '\t') l
    checkRev t = if isRev t then Just t else Nothing

-- | Run the "git" executable
runGit :: [T.Text] -> IO [T.Text]
runGit args = do
    (exitCode, sout, serr) <- readProcessWithExitCode "git" (T.unpack <$> args) ""
    case (exitCode, lines sout) of
      (ExitSuccess, ls)  -> pure $ T.pack <$> ls
      _ -> abortGitFailure args (T.pack sout) (T.pack serr)

isRev :: T.Text -> Bool
isRev t =
  -- commit hashes a comprised of abcdef0123456789
  T.all (\c -> (c >= 'a' && c <= 'f') || (c >= '0' && c <= '9')) t &&
  -- commit _should_ be 40 chars long, but to be sure we pick 7
  T.length t >= 7

abortTooMuchOutput :: [T.Text] -> IO a
abortTooMuchOutput = abort .  T.unwords

abortNoRef :: T.Text -> IO a
abortNoRef = abort -- TODO

abortNoRev :: T.Text -> IO a
abortNoRev = abort -- TODO

abortNoRefAndRev :: IO a
abortNoRefAndRev = error "foo"

-- TODO: mention error code
abortGitFailure :: [T.Text] -> T.Text -> T.Text -> IO a
abortGitFailure args sout serr = abort $ [s|
Could not read the output of 'git'. This is a bug. Please create a
ticket:

  https://github.com/nmattia/niv/issues/new

Thanks! I'll buy you a beer.
|] <> T.unlines ["command: ", T.unwords args, "stdout: ", sout, "stderr: ", serr]
