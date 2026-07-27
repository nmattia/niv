{-# LANGUAGE RankNTypes #-}

module Niv.Cmd where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Niv.Sources
import Niv.Update
import qualified Options.Applicative as Opts

data Cmd = Cmd
  { description :: forall a. Opts.InfoMod a,
    -- | Important: if an object is returned, then it should be accepted by 'acceptsCmd'
    parseCmdShortcut :: T.Text -> Maybe (PackageName, Aeson.Object),

    -- | Important: if an object is returned, then it should be accepted by 'acceptsCmd'
    parsePackageSpec :: Opts.Parser PackageSpec,
    updateCmd :: Update () (),
    name :: T.Text,
    -- | Some notes to print
    extraLogs :: Attrs -> [T.Text],
    -- | Returns True if this Cmd knows how to handle the
    -- given PackageSpec
    acceptsCmd :: PackageSpec -> Bool
  }
