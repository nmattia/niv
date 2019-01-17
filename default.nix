with { fetch = import ./nix/fetch.nix; };
let

  # The (pinned) Nixpkgs where the original packages are sourced from
  pkgs = import fetch.nixpkgs {};

  # The list of packages to be installed
  shellPackages = [ snack-exe ];

  snack-exe = (import fetch.snack).snack-exe;
  snack-lib = (import fetch.snack).snack-lib;
in
  if pkgs.lib.inNixShell
  then pkgs.mkShell
    { buildInputs = shellPackages;
    }
  else snack-lib.inferHPackBuild ./package.yaml
