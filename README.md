# niv

[![CircleCI](https://circleci.com/gh/nmattia/niv.svg?style=svg)](https://circleci.com/gh/nmattia/niv)

Painless dependencies for [Nix] projects. Read more in the [use case
section](#use-cases) section below.

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

Run the test suite with this command:

``` bash
$ ./script/test
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

This section covers common use cases:

* [Bootstrapping a Nix project](#bootstrapping-a-nix-project).
* [Tracking a different nixpkgs branch](#tracking-a-nixpkgs-branch).
* [Importing packages from GitHub](#importing-packages-from-github).
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

The file `nix/sources.json` is the file used by niv to store versions and is
initialized with niv and nixpkgs:

``` json
{
    "nixpkgs": {
        "url": "https://github.com/NixOS/nixpkgs-channels/archive/109a28ab954a0ad129f7621d468f829981b8b96c.tar.gz",
        "owner": "NixOS",
        "branch": "nixos-18.09",
        "url_template": "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz",
        "repo": "nixpkgs-channels",
        "sha256": "12wnxla7ld4cgpdndaipdh3j4zdalifk287ihxhnmrzrghjahs3q",
        "description": "Nixpkgs/NixOS branches that track the Nixpkgs/NixOS channels",
        "rev": "109a28ab954a0ad129f7621d468f829981b8b96c"
    },
    "niv": {
        "homepage": "https://github.com/nmattia/niv",
        "url": "https://github.com/nmattia/niv/archive/72e77204544527279e8f1e2d982d29503482b8f4.tar.gz",
        "owner": "nmattia",
        "branch": "master",
        "url_template": "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz",
        "repo": "niv",
        "sha256": "1zjcyzxhq9iwxh58j5d7sx1vz5s3r1f6gpmnfgj2a3rxmclwvn3c",
        "description": "Easy dependency management for Nix projects",
        "rev": "72e77204544527279e8f1e2d982d29503482b8f4"
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
  "url": "https://github.com/stedolan/jq/archive/9fa2e51099c55af56e3e541dc4b399f11de74abe.tar.gz",
  "owner": "stedolan",
  "branch": "master",
  "url_template": "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz",
  "repo": "jq",
  "sha256": "0819rvk8057qgcqvgn7fpldvly2pfdw9fxcjrlqa8gr59p8a1cic",
  "description": "Command-line JSON processor",
  "rev": "9fa2e51099c55af56e3e541dc4b399f11de74abe"
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
NIV - Version manager for Nix projects

Usage: niv COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  init                     Initialize a Nix project. Existing files won't be
                           modified.
  add                      Add dependency
  show                     
  update                   Update dependencies
  drop                     Drop dependency

```

#### Add

```
Examples:

  niv add stedolan/jq
  niv add NixOS/nixpkgs-channels -n nixpkgs -b nixos-18.09
  niv add my-package -v alpha-0.1 -t http://example.com/archive/<version>.zip

Usage: niv add [-n|--name NAME] PACKAGE ([-a|--attribute KEY=VAL] |
               [-b|--branch BRANCH] | [-o|--owner OWNER] | [-r|--repo REPO] |
               [-v|--version VERSION] | [-t|--template URL])
  Add dependency

Available options:
  -n,--name NAME           Set the package name to <NAME>
  -a,--attribute KEY=VAL   Set the package spec attribute <KEY> to <VAL>
  -b,--branch BRANCH       Equivalent to --attribute branch=<BRANCH>
  -o,--owner OWNER         Equivalent to --attribute owner=<OWNER>
  -r,--repo REPO           Equivalent to --attribute repo=<REPO>
  -v,--version VERSION     Equivalent to --attribute version=<VERSION>
  -t,--template URL        Used during 'update' when building URL. Occurrences
                           of <foo> are replaced with attribute 'foo'.
  -h,--help                Show this help text

```

#### Update

```
Examples:

  niv update
  niv update nixpkgs
  niv update my-package -v beta-0.2

Usage: niv update [PACKAGE] ([-a|--attribute KEY=VAL] | [-b|--branch BRANCH] |
                  [-o|--owner OWNER] | [-r|--repo REPO] | [-v|--version VERSION]
                  | [-t|--template URL])
  Update dependencies

Available options:
  -a,--attribute KEY=VAL   Set the package spec attribute <KEY> to <VAL>
  -b,--branch BRANCH       Equivalent to --attribute branch=<BRANCH>
  -o,--owner OWNER         Equivalent to --attribute owner=<OWNER>
  -r,--repo REPO           Equivalent to --attribute repo=<REPO>
  -v,--version VERSION     Equivalent to --attribute version=<VERSION>
  -t,--template URL        Used during 'update' when building URL. Occurrences
                           of <foo> are replaced with attribute 'foo'.
  -h,--help                Show this help text

```

#### Drop

```
Examples:

  niv drop jq
  niv drop my-package version

Usage: niv drop PACKAGE [ATTRIBUTE]
  Drop dependency

Available options:
  -h,--help                Show this help text

```

#### Init

```
Usage: niv init 
  Initialize a Nix project. Existing files won't be modified.

Available options:
  -h,--help                Show this help text

```

#### show

```
Usage: niv show 

Available options:
  -h,--help                Show this help text

```

## Related

* [nix-flakes]: `niv` support a subset of the Nix flakes. In particular it does
  not perform any kind of dependency resolution.
* [nix-path]: `niv` and `nix-path` share a similar goal and ideas tend to flow
  back and forth freely.

[Nix]: https://nixos.org/nix/
[jq]: https://stedolan.github.io/jq/
[GHC]: https://www.haskell.org/ghc/
[nix-flakes]: https://gist.github.com/edolstra/40da6e3a4d4ee8fd019395365e0772e7
[nix-path]: https://github.com/zimbatm/nix-path
