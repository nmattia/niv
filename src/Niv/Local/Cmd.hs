{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Niv.Local.Cmd where

import Control.Arrow
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import Niv.Cmd
import Niv.Sources
import Niv.Update
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Help.Pretty as Opts

localCmd :: Cmd
localCmd =
  Cmd
    { description = describeLocal,
      parseCmdShortcut = parseLocalShortcut,
      parsePackageSpec = parseLocalPackageSpec,
      updateCmd = proc () -> do
        useOrSet "type" -< ("local" :: Box T.Text)
        returnA -< (),
      name = "local",
      extraLogs = const []
    }

parseLocalShortcut :: T.Text -> Maybe (PackageName, Aeson.Object)
parseLocalShortcut txt =
  if T.isPrefixOf "./" txt || T.isPrefixOf "/" txt
    then do
      let n = last $ T.splitOn "/" txt
      Just (PackageName n, KM.fromList [("path", Aeson.String txt)])
    else Nothing

parseLocalPackageSpec :: Opts.Parser PackageSpec
parseLocalPackageSpec = PackageSpec . KM.fromList <$> parseParams
  where
    parseParams :: Opts.Parser [(K.Key, Aeson.Value)]
    parseParams = maybe [] pure <$> Opts.optional parsePath
    parsePath =
      ("path",) . Aeson.String
        <$> Opts.strOption
          ( Opts.long "path"
              <> Opts.metavar "PATH"
          )

describeLocal :: Opts.InfoMod a
describeLocal =
  mconcat
    [ Opts.fullDesc,
      Opts.progDesc "Add a local dependency. Experimental.",
      Opts.headerDoc $
        Just $
          Opts.vcat
            [ "Examples:",
              "",
              "  niv add local ./foo/bar"
            ]
    ]
