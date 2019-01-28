# niv

A tool for dealing with third-party packages in [Nix].

## Building

Inside the provided nix shell:

``` bash
$ # GHCi:
$ snack ghci
$ # run:
$ snack run -- <args>
```

## Usage

* [Add](#add)
* [Update](#update)
* [Drop](#drop)

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


### Add

```
Examples:

  niv add stedolan/jq
  niv add NixOS/nixpkgs-channel -n nixpkgs -b nixos-18.09
  niv add my-package -v alpha-0.1 -t http://example.com/archive/<version>.zip

Usage: niv add PACKAGE ([-b|--branch BRANCH] | [-o|--owner OWNER] |
                         [-r|--repo REPO] | [-v|--version VERSION] |
                         [-a|--attribute KEY=VAL] | [-t|--template URL])
                         [-n|--name NAME]
  Add dependency

Available options:
  -t,--template URL        foo
  -h,--help                Show this help text
```


### Update

```
Examples:

  niv update
  niv update nixpkgs
  niv update my-package -v beta-0.2

Usage: niv update [PACKAGE] ([-b|--branch BRANCH] | [-o|--owner OWNER]
                            | [-r|--repo REPO] | [-v|--version VERSION] |
                            [-a|--attribute KEY=VAL] | [-t|--template URL])
  Update dependencies

Available options:
  -t,--template URL        foo
  -h,--help                Show this help text
```

### Drop

```
Examples:

  niv drop jq

Usage: niv drop PACKAGE
  Drop dependency

Available options:
  -h,--help                Show this help text
```

[Nix]: https://nixos.org/nix/
