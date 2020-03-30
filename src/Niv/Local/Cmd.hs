{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Niv.Local.Cmd where

import Niv.Cmd
import Control.Arrow
import Niv.Sources
import Niv.Update
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Help.Pretty as Opts

localCmd :: Cmd
localCmd = Cmd
  { description = describeLocal
  , parseCmdShortcut = parseLocalShortcut
  , parsePackageSpec = parseLocalPackageSpec
  , updateCmd = proc () -> do
      useOrSet "type" -< ("local" :: Box T.Text)
      returnA -< ()
  , name = "local"
  }

parseLocalShortcut :: T.Text -> Maybe (PackageName, Aeson.Object)
parseLocalShortcut txt =
    if (T.isPrefixOf "./" txt || T.isPrefixOf "/" txt ) then do
      let n = last $ T.splitOn "/" txt
      Just (PackageName n, HMS.fromList [ ("path", Aeson.String txt) ])
    else Nothing

parseLocalPackageSpec :: Opts.Parser PackageSpec
parseLocalPackageSpec = PackageSpec . HMS.fromList <$> parseParams
  where
    parseParams :: Opts.Parser [(T.Text, Aeson.Value)]
    parseParams = maybe [] pure <$> Opts.optional parsePath

    parsePath =
      ("path", ) . Aeson.String <$> Opts.strOption
        ( Opts.long "path" <>
          Opts.metavar "PATH"
        )

describeLocal :: Opts.InfoMod a
describeLocal = mconcat
  [ Opts.fullDesc
  , Opts.progDesc "Add a local dependency. Experimental."
  , Opts.headerDoc $ Just $
      "Examples:" Opts.<$$>
      "" Opts.<$$>
      "  niv add local ./foo/bar"
  ]
