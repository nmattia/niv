{-# LANGUAGE OverloadedStrings #-}

module Niv.Git.Cmd (gitCmd) where

import Niv.Cmd
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Help.Pretty as Opts

gitCmd :: Cmd
gitCmd = Cmd
  { description = describeGit
  , parseCmdShortcut = pure Nothing
  , parsePackageSpec = pure mempty
  , updateCmd = error "git update is not implemented yet"
  , name = "git"
  }

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

-- for git:
--  default branch:
--  ~/niv$ git ls-remote --symref git@github.com:NixOS/nixpkgs HEAD
--  ref: refs/heads/master  HEAD
--  3dd8e8e7faa87fc45c2492f88643bb363572180e        HEAD
--  0a46a71a6ec41764b118a24e4cbf1b4bc4be906e        refs/remotes/origin/HEAD
--
--  lastest rev:
--  ~/niv$ git ls-remote git@github.com:NixOS/nixpkgs refs/heads/master
--  3dd8e8e7faa87fc45c2492f88643bb363572180e        refs/heads/master
