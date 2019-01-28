with { pkgs = import ./nix; };
pkgs.snack-lib.inferHPackBuild ./package.yaml
