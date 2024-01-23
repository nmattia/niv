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

import Control.Applicative
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Bifunctor
import qualified Data.ByteString.Char8 as B8
import Data.Char (isAlphaNum)
import Data.Maybe
import Data.String.QQ (s)
import qualified Data.Text as T
import Data.Text.Extended
import Niv.Cmd
import Niv.GitHub
import Niv.GitHub.API
import Niv.Sources
import Niv.Update
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Help.Pretty as Opts
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)

githubCmd :: Cmd
githubCmd =
  Cmd
    { description = describeGitHub,
      parseCmdShortcut = parseAddShortcutGitHub,
      parsePackageSpec = parseGitHubPackageSpec,
      updateCmd = githubUpdate',
      name = "github",
      extraLogs = const []
      -- TODO: here filter by type == tarball or file or builtin-
    }

parseGitHubPackageSpec :: Opts.Parser PackageSpec
parseGitHubPackageSpec =
  (PackageSpec . KM.fromList)
    <$> many parseAttribute
  where
    parseAttribute :: Opts.Parser (K.Key, Aeson.Value)
    parseAttribute =
      Opts.option
        (Opts.maybeReader parseKeyValJSON)
        ( Opts.long "attribute"
            <> Opts.short 'a'
            <> Opts.metavar "KEY=VAL"
            <> Opts.help "Set the package spec attribute <KEY> to <VAL>, where <VAL> may be JSON."
        )
        <|> Opts.option
          (Opts.maybeReader (parseKeyVal Aeson.toJSON))
          ( Opts.long "string-attribute"
              <> Opts.short 's'
              <> Opts.metavar "KEY=VAL"
              <> Opts.help "Set the package spec attribute <KEY> to <VAL>."
          )
        <|> shortcutAttributes
        <|> ( (("url_template",) . Aeson.String)
                <$> Opts.strOption
                  ( Opts.long "template"
                      <> Opts.short 't'
                      <> Opts.metavar "URL"
                      <> Opts.help "Used during 'update' when building URL. Occurrences of <foo> are replaced with attribute 'foo'."
                  )
            )
        <|> ( (("type",) . Aeson.String)
                <$> Opts.strOption
                  ( Opts.long "type"
                      <> Opts.short 'T'
                      <> Opts.metavar "TYPE"
                      <> Opts.help "The type of the URL target. The value can be either 'file' or 'tarball'. If not set, the value is inferred from the suffix of the URL."
                  )
            )
    parseKeyValJSON = parseKeyVal $ \x ->
      fromMaybe (Aeson.toJSON x) (Aeson.decodeStrict (B8.pack x))
    -- Parse "key=val" into ("key", val)
    parseKeyVal ::
      -- how to convert to JSON
      (String -> Aeson.Value) ->
      String ->
      Maybe (K.Key, Aeson.Value)
    parseKeyVal toJSON str = case span (/= '=') str of
      (key, '=' : val) -> Just (K.fromString key, toJSON val)
      _ -> Nothing
    -- Shortcuts for common attributes
    shortcutAttributes :: Opts.Parser (K.Key, Aeson.Value)
    shortcutAttributes =
      foldr (<|>) empty $
        mkShortcutAttribute
          <$> ["branch", "owner", "rev", "version"]
    -- TODO: infer those shortcuts from 'Update' keys
    mkShortcutAttribute :: T.Text -> Opts.Parser (K.Key, Aeson.Value)
    mkShortcutAttribute = \case
      attr@(T.uncons -> Just (c, _)) ->
        fmap (second Aeson.String) $
          (K.fromText attr,)
            <$> Opts.strOption
              ( Opts.long (T.unpack attr)
                  <> Opts.short c
                  <> Opts.metavar (T.unpack $ T.toUpper attr)
                  <> Opts.help
                    ( T.unpack $
                        "Equivalent to --attribute "
                          <> attr
                          <> "=<"
                          <> (T.toUpper attr)
                          <> ">"
                    )
              )
      _ -> empty

describeGitHub :: Opts.InfoMod a
describeGitHub =
  mconcat
    [ Opts.fullDesc,
      Opts.progDesc "Add a GitHub dependency",
      Opts.headerDoc $
        Just $
          Opts.vcat
            [ "Examples:",
              "",
              "  niv add stedolan/jq",
              "  niv add NixOS/nixpkgs -n nixpkgs -b nixpkgs-unstable",
              "  niv add my-package -v alpha-0.1 -t http://example.com/archive/<version>.zip"
            ]
    ]

-- parse a github shortcut of the form "owner/repo"
parseAddShortcutGitHub :: T.Text -> Maybe (PackageName, Aeson.Object)
parseAddShortcutGitHub str =
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
    args = (if unpack then ["--unpack"] else []) <> [url, "--name", sanitizeName basename]
    runNixPrefetch = readProcessWithExitCode "nix-prefetch-url" args ""
    sanitizeName = T.unpack . T.filter isOk
    basename = last $ T.splitOn "/" turl
    -- From the nix-prefetch-url documentation:
    --  Path names are alphanumeric and can include the symbols +-._?= and must
    --  not begin with a period.
    -- (note: we assume they don't begin with a period)
    isOk = \c -> isAlphaNum c || T.any (c ==) "+-._?="

abortNixPrefetchExpectedOutput :: [T.Text] -> T.Text -> T.Text -> IO a
abortNixPrefetchExpectedOutput args sout serr =
  abort $
    [s|
Could not read the output of 'nix-prefetch-url'. This is a bug. Please create a
ticket:

  https://github.com/nmattia/niv/issues/new

Thanks! I'll buy you a beer.
|]
      <> T.unlines ["command: ", "nix-prefetch-url" <> T.unwords args, "stdout: ", sout, "stderr: ", serr]
