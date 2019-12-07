# Changelog

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
