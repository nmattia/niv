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

```
replace_niv_help
```

### Add

```
replace_niv_add_help
```

### Update

```
replace_niv_update_help
```

### Drop

```
replace_niv_drop_help
```

### Init

```
replace_niv_init_help
```

### show

```
replace_niv_show_help
```

[Nix]: https://nixos.org/nix/
