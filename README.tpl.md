# niv

[![CircleCI](https://circleci.com/gh/nmattia/niv.svg?style=svg)](https://circleci.com/gh/nmattia/niv)
[![Netlify Status](https://api.netlify.com/api/v1/badges/48532eaa-259f-4ca2-aadf-67f7c6b957fd/deploy-status)](https://niv.nmattia.com)

Painless dependencies for [Nix] projects. Read more in the [Getting started](#getting-started) section below.

<p align="center">
    <img src="https://niv.nmattia.com/niv.svg">
</p>

## Install

``` bash
$ nix-env -iA niv -f https://github.com/nmattia/niv/tarball/master
```

## Build

Inside the provided nix shell:

``` bash
$ repl
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

* [Init](#init): bootstraps a Nix project, in particular creates a
  `nix/sources.json` file containing `niv` and `nixpkgs` as well as a
  `nix/sources.nix` file that returns the sources as a Nix object.
* [Show](#show): shows the packages' information.

The next two sections cover [common use cases](#getting-started) and [full command
description](#commands).

### Configuration

The following environment variables are read by `niv`:

| Name            | Note |
| --------------- | ---- |
| GITHUB_TOKEN    | When set, the value is used to authenticate GitHub API requests. |
| GITHUB_HOST     | The GitHub host to use when fetching packages. Port may be appended here. |
| GITHUB_API_HOST | The host used when performing GitHub API requests. Use `GITHUB_API_PORT` for specifying the port. |
| GITHUB_API_PORT | The port used when performing GitHub API requests. Defauts to `443` for secure requests. Defaults to `80` for insecure requests. See also: `GITHUB_SECURE`. |
| GITHUB_INSECURE | When set to anything but the empty string, requests are performed over `http` instead of `https`. |
| GITHUB_PATH     | The base path used when performing GitHub API requests. |

The next two sections cover [common use cases](#getting-started) and [full command
description](#commands).

### Getting started

This section covers common use cases:

* [Bootstrapping a Nix project](#bootstrapping-a-nix-project).
* [Tracking a different nixpkgs branch](#tracking-a-nixpkgs-branch).
* [Importing packages from GitHub](#importing-packages-from-github).
* [Fetching packages from custom URLs](#using-custom-urls).

#### Bootstrapping a Nix project

Use the `init` command when starting a new Nix project or when porting an
existing Nix project to niv:

``` shell
$ niv init
...
$ tree
.
└── nix
    ├── sources.json
    └── sources.nix

1 directory, 2 files
```

The file `nix/sources.json` is the file used by niv to store versions and is
initialized with niv and nixpkgs:

``` json
{
    "nixpkgs": {
        "url": "https://github.com/NixOS/nixpkgs-channels/archive/109a28ab954a0ad129f7621d468f829981b8b96c.tar.gz",
        "owner": "NixOS",
        "branch": "nixos-19.03",
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

To use those dependencies `import` the file `nix/sources.nix`, e.g.:

``` nix
{ sources ? import ./sources.nix }:     # import the sources
with
  { overlay = _: pkgs:
      { niv = import sources.niv {};    # use the sources :)
      };
  };
import sources.nixpkgs                  # and use them again!
  { overlays = [ overlay ] ; config = {}; }
```

#### Tracking a nixpkgs branch

The `init` command sets the `nix/sources.json` file to track the latest commit
present on nixpkgs 19.03 when the command was run. Run the following command to
update it:

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
