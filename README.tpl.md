# niv

[![CircleCI](https://circleci.com/gh/nmattia/niv.svg?style=svg)](https://circleci.com/gh/nmattia/niv)

A tool for dealing with third-party packages in [Nix]. Read more about it in
the [usage](#usage) section.

## Install

``` bash
$ nix-env -iA niv -f https://github.com/nmattia/niv/tarball/master
```

## Build

Inside the provided nix shell:

``` bash
$ # GHCi:
$ snack ghci
$ # run:
$ snack run -- <args>
```

## Usage

`niv` simplifies [adding](#add) and [updating](#update) dependencies in Nix
projects. It uses a single file, `nix/sources.json`, where it stores the data
necessary for fetching and updating the packages.

* [Add](#add): inserts a package in `nix/sources.json`.
* [Update](#update): updates one or all packages in `nix/sources.json`.
* [Drop](#drop): deletes a package from `nix/sources.json`.

`niv` has two more utility functions:

* [Init](#init): bootstraps a Nix projects, in particular creates a
  `nix/sources.json` file containing `niv` and `nixpkgs` as well as a
  `nix/sources.nix` file that returns the sources as a Nix object.
* [Show](#show): shows the packages' information.

The next two sections cover [common use cases](#use-cases) and [full command
description](#commands).

### Use cases

This covers three use cases:

* [Bootstrapping a Nix project](#bootstrapping-a-nix-project).
* [Tracking a different a different nixpkgs branch](#tracking-a-nixpkgs-branch).
* [Fetching packages from custom URLs](#using-custom-urls).

#### Bootstrapping a Nix project

There is an `init` command that is useful when starting a new Nix project or
when porting an existing Nix project to the niv versioning:

``` shell
$ niv init
...
$ tree
.
├── default.nix
├── nix
│   ├── default.nix
│   ├── packages.nix
│   ├── sources.json
│   └── sources.nix
└── shell.nix

1 directory, 6 files
```

The files `default.nix`, `nix/default.nix` and `nix/packages.nix` are useful
defaults for any Nix project. The file `nix/sources.json` is the file used by
niv to store versions and is initialized with niv and nixpkgs:

``` json
{
    "nixpkgs": {
        "url": "https://github.com/NixOS/nixpkgs-channels/archive/....tar.gz",
        "owner": "NixOS",
        "branch": "nixos-18.09",
        "url_template": "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz",
        "repo": "nixpkgs-channels",
        "sha256": "...",
        "description": "Nixpkgs/NixOS branches that track the Nixpkgs/NixOS channels",
        "rev": "..."
    },
    "niv": {
        "homepage": "https://github.com/nmattia/niv",
        "url": "https://github.com/nmattia/niv/archive/....tar.gz",
        "owner": "nmattia",
        "branch": "master",
        "url_template": "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz",
        "repo": "niv",
        "sha256": "...",
        "description": "Manager for third-party packages in Nix",
        "rev": "..."
    }
}
```

The content of this file can be used from Nix by importing `nix/sources.nix` as
done in the generated `nix/default.nix` file:

``` nix
{ sources ? import ./sources.nix }:
with
  { overlay = _: pkgs:
      { inherit (import sources.niv {}) niv;
        packages = pkgs.callPackages ./packages.nix {};
      };
  };
import sources.nixpkgs
  { overlays = [ overlay ] ; config = {}; }
```

The files `default.nix`, `shell.nix` and `nix/packages.nix` are placeholders
for your project.

#### Tracking a nixpkgs branch

The `init` command sets the `nix/sources.json` file to track the latest commit
present on nixpkgs 18.09 when the command was run. Run this commit to track
update to the latest commit:

``` shell
$ niv update nixpkgs
```

To change the branch being tracked run this command:

``` shell
$ niv update nixpkgs -b nixos-19.03     # equivalent to --branch nixos-19.03
```

#### Importing packages from GitHub

The `add` command will infer information about the package being added, when
possible. This works very well for GitHub repositories. Run this command to add
[jq] to your project:


``` shell
$ niv add stedolan/jq
```

The following data was added in `nix/sources.json` for `jq`:

``` json
{
  "homepage": "http://stedolan.github.io/jq/",
  "url": "https://github.com/stedolan/jq/archive/....tar.gz",
  "owner": "stedolan",
  "branch": "master",
  "url_template": "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz",
  "repo": "jq",
  "sha256": "...",
  "description": "Command-line JSON processor",
  "rev": "..."
}
```

#### Using custom URLs

It is possible to use niv to fetch packages from custom URLs. Run this command
to add the Haskell compiler [GHC] to your `nix/sources.json`:

``` shell
$ niv add ghc   \
    -v 8.4.3    \
    -t 'https://downloads.haskell.org/~ghc/<version>/ghc-<version>-i386-deb8-linux.tar.xz'
```

The option `-v` sets the "version" attribute to `8.4.3`. The option `-t` sets a
template that can be reused by niv when fetching a new URL (see the
documentation for [add](#add) and [update](#update)).

For updating the version of GHC used run this command:

``` shell
$ niv update ghc -v 8.6.2
```

### Commands

```
replace_niv_help
```

#### Add

```
replace_niv_add_help
```

#### Update

```
replace_niv_update_help
```

#### Drop

```
replace_niv_drop_help
```

#### Init

```
replace_niv_init_help
```

#### show

```
replace_niv_show_help
```

[Nix]: https://nixos.org/nix/
[jq]: https://stedolan.github.io/jq/
[GHC]: https://www.haskell.org/ghc/
