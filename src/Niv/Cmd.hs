{-# LANGUAGE RankNTypes #-}

module Niv.Cmd where

import Niv.Sources
import Niv.Update
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Options.Applicative as Opts

-- TODO: add filter
data Cmd = Cmd
  { description :: forall a. Opts.InfoMod a
  -- TODO: should be "Maybe"
  , parseShortcut :: T.Text -> (PackageName, Aeson.Object)
  , parsePackageSpec :: Opts.Parser PackageSpec
  , updateCmd :: Update () ()
  , name :: T.Text
  }
