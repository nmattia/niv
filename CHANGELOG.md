# Changelog

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
