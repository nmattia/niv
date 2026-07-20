{-# LANGUAGE RankNTypes #-}

module Niv.Cmd where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Niv.Sources
import Niv.Update
import qualified Options.Applicative as Opts

data Cmd = Cmd
  { description :: forall a. Opts.InfoMod a,
    parseCmdShortcut :: T.Text -> Maybe (PackageName, Aeson.Object),
    parsePackageSpec :: Opts.Parser PackageSpec,
    updateCmd :: Update () (),
    name :: T.Text,
    -- | Some notes to print
    extraLogs :: Attrs -> [T.Text],
    -- | Returns True if this Cmd knows how to handle the
    -- given PackageSpec
    acceptsCmd :: PackageSpec -> Bool
  }
