# Changelog

## [0.2.22] 2023-03-12

## Changed
* Add statix sugegstions

## [0.2.21] 2022-05-06

## Added
* Fetch submodules (if supported, otherwise warn)

## [0.2.20] 2022-04-04

## Added
* Add ability to pass submodules to `builtins.fetchGit`

## Changed
* Remove warnings about GITHUB_INSECURE and GITHUB_PATH
* Support newer GHC
* Update optparse-applicative and aeson (aeson 2)
* Update nixpkgs and track master
* Speed up initialization (skip `nix-prefetch-url`)
* Bunch of documentation fixes
* Fix badges and GitHub Actions


## [0.2.19] 2021-01-05
## Added
* There is a new flag `--no-colors` for avoiding colors in the output
## Changed
* `GITHUB_PATH` was renamed to `NIV_GITHUB_PATH` https://github.com/nmattia/niv/issues/280
* `GITHUB_INSECURE` was renamed to `NIV_GITHUB_INSECURE` https://github.com/nmattia/niv/issues/280
* If `-T` is provided on the command line for `niv add` it will be respected and no guessing from the url is done.
* `type` attribute is now respected.
* If an url template is updated, `type` needs to be adjusted manuall as it is not guessed from the new url template.

## [0.2.17] 2020-09-08
## Added
* There is a new flag `-r/--rev` for specifying a revision during update and add
* There is a new flag `--version` that instructs niv to show its version and exit
## Changed
* The derivation name of sources is sanitized
* The `ref` field was deprecated in favor of `branch` and `tag` in `git`
  sources

## [0.2.16] 2020-08-14
## Changed
* The `sources.nix` can now be imported when there's no local `sources.json`

## [0.2.15] 2020-08-13
## Added
* The sources can be overriden with `NIV_OVERRIDE_<source name>`
## Changed
* When `nix-prefetch-url` fails the command is shown
* IO operations during update are cached

## [0.2.14] 2020-07-15
## Added
* `niv add local` for local sources.
* Custom nixpkgs can be specified during `init`.
## Changed
* The derivation name for package `foo` is now `foo-src`.
* The extension `.tgz` is considered as `.tar.gz`.
* The default nixpkgs is `release-20.03`.
* Nixpkgs is now pulled from `NixOS/nixpkgs`, not from channels.
## Removed
* The types `builtin-tarbal` and `builtin-url` were removed.

## [0.2.13] 2020-02-02
## Added
* `niv modify -n NAME` to rename a package
## Changed
* README mentions the `modify` command
* `niv` is not shipped with the `niv-test` executable anymore
* `cabal-upload` was simplified

## [0.2.12] 2020-01-17
## Added
* Examples for building C++ libraries with niv
* Documentation for using niv from nixpkgs
## Changed
* Only depend on executables in `default.nix` (`-A niv`) for smaller closure
  size
* Ensure `<nixpkgs>` is not evaluated in `sources.nix` unless necessary

## [0.2.11] 2020-01-07
## Changed
* Users can set custom `pkgs` when `import`ing `sources.nix`

## [0.2.10] 2020-01-06
## Changed
* The bundled `nix/sources.nix` is formatted with `nixpkgs-fmt`

## [0.2.9] 2019-12-17
## Changed
* `niv init` uses nixpkgs 19.09

## [0.2.8] 2019-12-09
## Changed
* Fixed message in `niv init` with custom `sources.json`

## [0.2.7] 2019-12-08
## Added
* Support for custom path `sources.json` with `--sources-json`

## [0.2.6] 2019-12-05
## Changed
* Fix `niv update` with `git` specs

## [0.2.5] 2019-12-01
## Changed
* Fix `niv show` adding extra newlines

## [0.2.4] 2019-12-01
### Added
* Experimental support for `add` subcommands, in particular `niv add git`
## Changed
* Various error message fixes

## [0.2.3] 2019-11-28
### Added
* A new CLI option (`-s`) reads attributes as raw strings.
### Changed
* The attribute CLI option (`-a`) now allows JSON value.
* Some typos were fixed.
* A deprecation warning was added for `builtin-tarball`.

## [0.2.2] 2019-11-27
### Added
* The `sources.nix` are now versioned.
* Show the help when no arguments are provided.
### Changed
* The `show` command was prettified.
