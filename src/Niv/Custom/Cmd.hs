{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE QuasiQuotes #-}

module Niv.Custom.Cmd where

import Control.Applicative
import Control.Arrow
import Data.Maybe
import Data.Text.Extended as T
import Niv.Cmd
import Niv.Logger
import Niv.Sources
import Niv.Update
import System.Exit (ExitCode(ExitSuccess))
import System.Process (readProcessWithExitCode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Help.Pretty as Opts

customCmd :: Cmd
customCmd = Cmd
  { description = describeCustom
  , parseCmdShortcut = const Nothing
  , parsePackageSpec = undefined
  , updateCmd = customUpdate'
  , name = "custom"
  }

describeCustom :: Opts.InfoMod a
describeCustom = mconcat
  [ Opts.fullDesc
  , Opts.progDesc "Add a custom dependency. Experimental."
  , Opts.headerDoc $ Just $
      "Examples:" Opts.<$$>
      "" Opts.<$$>
      "  niv add custom --path ./script/update-dep"
  ]

parseCustomPackageSpec :: Opts.Parser PackageSpec
parseCustomPackageSpec = pure $ PackageSpec HMS.empty

customUpdate :: (FilePath -> Aeson.Object -> IO Aeson.Object) -> Update () ()
customUpdate runExe = proc () -> do
    path <- load "path" -< ()
    vals <- Read -< ()
    newVal <- run' (uncurry runExe) -< (,) <$> path <*> pure vals
    override -< newVal

customUpdate' :: Update () ()
customUpdate' = customUpdate runExe
  where
    runExe fp obj = do
      (exitCode, sout, serr) <- readProcessWithExitCode fp [ BL8.unpack $ Aeson.encode obj ] ""
      case (exitCode, lines sout) of
        (ExitSuccess, [l])  -> case Aeson.decodeStrict (B8.pack l) of
          Just (Aeson.Object obj) -> pure obj
        _ -> error $ show $ T.unlines
          [ T.unwords [ "stdout:" , T.pack sout ]
          , T.unwords [ "stderr:" , T.pack serr ]
          ]
