{-# LANGUAGE OverloadedStrings #-}

module Niv.Git.Cmd (gitCmd) where

import Niv.Cmd
import qualified Options.Applicative as Opts

gitCmd :: Cmd
gitCmd = Cmd
  { description = describeGit
  , parseShortcut = error "no parse for git"
  , parsePackageSpec = pure mempty
  , updateCmd = undefined
  , name = "git"
  }

describeGit :: Opts.InfoMod a
describeGit = mconcat
  [ Opts.fullDesc
  , Opts.progDesc "Echo a message back"
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
