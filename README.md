# niv

A tool for dealing with third-party packages in [Nix].

## Usage

### Global options

* `--versions`: path to the `vesions.json`

**NOTES**

* no support for non-json, to enforce convention

### Commands

#### init

[--fetch]

Creates (if the file doesn't exist)

* `nix/versions.json`:
``` json
{"nixpkgs": { ... }}
```

*`nix/fetch.nix`:
``` nix
...
```

*`default.nix`:
``` nix
with { fetch = import <fetch>; };
let pkgs = import fetch.nixpkgs;
in pkgs.hello
```

#### show

`[--branch] [--rev] [--owner] [--repo] <p1> <p2>`... if no attribute
  (br, rev, own, repo) is given, all attributes are shown for `<packages>`.
  Otherwise the specified attributes are shown. If no package is specified:
  `<packages> = <all packages>`, otherwise `<packages>` is set to the specified
  packages.

#### update

* `[p [--commit] [--branch]]`
 - `[]`: all packages are updated
 - `[p1 p2 ...]`: the specified packages are updated

* `--commit <rev>`: `rev` is set to `<rev>` and the package is prefetched
* `--branch <branch>`: `branch` is set to `<branch>`, `rev` is set to the
  latest revision on that branch and the package is prefetched

#### add

* `<package>`: adds the following to the versions file where `let <username/repo> = <package>`
``` json
{ "<repo>":
  { "owner":  "<username>",
    "repo":   "<repo>",
    "rev":    "<latest commit on <branch>>",
    "sha256": "<sha256>",
    "branch": "<branch>"
  }
}
```

* `--branch`: specifies `<branch>`
* `--username <username>`: then `let <repo> = <package>`
* `--gitlab`: use gitlab instead of GitHub

**NOTE**: should the URLs be used instead? or more simply, how do we differentiate between Gitlab/GitHub?

[Nix]: https://nixos.org/nix/
