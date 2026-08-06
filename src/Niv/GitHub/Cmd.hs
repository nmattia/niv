{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Niv.GitHub.Cmd
  ( githubCmd,
  )
where

import Data.Aeson ((.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Bifunctor
import Data.Char (isAlphaNum)
import Data.String.QQ (s)
import qualified Data.Text as T
import Niv.Cmd
import Niv.GitHub
import Niv.GitHub.API
import Niv.Sources
import Niv.Update
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)

githubCmd :: Cmd
githubCmd =
  Cmd
    { parseCmdShortcut = parseAddShortcutGitHub,
      updateCmd = githubUpdate',
      name = "github",
      extraLogs = const [],
      acceptsCmd = \(unPackageSpec -> spec) ->
        (KM.member "repo" spec && KM.member "owner" spec) || KM.member "url_template" spec
    }

-- parse a github shortcut of the form "owner/repo"
parseAddShortcutGitHub :: T.Text -> Maybe (PackageName, PackageSpec)
parseAddShortcutGitHub str = second PackageSpec <$>
  -- parses a string "owner/repo" into package name (repo) and spec (owner +
  -- repo)
  case T.span (/= '/') str of
    ( owner@(T.null -> False),
      T.uncons -> Just ('/', repo@(T.null -> False))
      ) ->
        Just
          ( PackageName repo,
            KM.fromList ["owner" .= owner, "repo" .= repo]
          )
    -- XXX: this should be "Nothing" but for the time being we keep
    -- backwards compatibility with "niv add foo" adding "foo" as a
    -- package name.
    _ -> Just (PackageName str, KM.empty)

-- | The IO (real) github update
githubUpdate' :: Update () ()
githubUpdate' = githubUpdate nixPrefetchURL githubLatestRev githubRepo

nixPrefetchURL :: Bool -> T.Text -> IO T.Text
nixPrefetchURL unpack turl@(T.unpack -> url) = do
  (exitCode, sout, serr) <- runNixPrefetch
  case (exitCode, lines sout) of
    (ExitSuccess, l : _) -> pure $ T.pack l
    _ -> abortNixPrefetchExpectedOutput (T.pack <$> args) (T.pack sout) (T.pack serr)
  where
    args = (["--unpack" | unpack]) <> [url, "--name", sanitizeName basename]
    runNixPrefetch = readProcessWithExitCode "nix-prefetch-url" args ""
    sanitizeName = T.unpack . T.filter isOk
    basename = last $ T.splitOn "/" turl
    -- From the nix-prefetch-url documentation:
    --  Path names are alphanumeric and can include the symbols +-._?= and must
    --  not begin with a period.
    -- (note: we assume they don't begin with a period)
    isOk c = isAlphaNum c || T.any (c ==) "+-._?="

abortNixPrefetchExpectedOutput :: [T.Text] -> T.Text -> T.Text -> IO a
abortNixPrefetchExpectedOutput args sout serr =
  abort $
    [s|
Could not read the output of 'nix-prefetch-url'. This is a bug. Please create a
ticket:

  https://github.com/nmattia/niv/issues/new

Thanks! I'll buy you a beer.
|]
      <> T.unlines ["command: ", T.unwords ("nix-prefetch-url" : args), "stdout: ", sout, "stderr: ", serr]
