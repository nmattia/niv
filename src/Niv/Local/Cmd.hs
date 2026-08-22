{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Niv.Local.Cmd where

import Control.Arrow
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import Niv.Cmd
import Niv.Sources
import Niv.Update

localCmd :: Cmd
localCmd =
  Cmd
    { parseCmdShortcut = parseLocalShortcut,
      updateCmd = proc () -> do
        useOrSet "type" -< ("local" :: Box T.Text)
        returnA -< (),
      name = "local",
      extraLogs = const [],
      acceptsCmd = \(unPackageSpec -> spec) -> KM.lookup "type" spec == Just "local"
    }

parseLocalShortcut :: T.Text -> Maybe (PackageName, PackageSpec)
parseLocalShortcut txt =
  second PackageSpec
    <$> if T.isPrefixOf "./" txt || T.isPrefixOf "/" txt
      then do
        let n = last $ T.splitOn "/" txt
        Just (PackageName n, KM.fromList [("path", Aeson.String txt), ("type", Aeson.String "local")])
      else Nothing
