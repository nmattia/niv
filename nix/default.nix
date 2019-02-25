{ sources ? import ./sources.nix }:
import sources.nixpkgs
  { overlays =
    [
      # Snack
      (_: pkgs:
        with
        { snack = pkgs.callPackage "${sources.snack}/nix/packages.nix" {}; };
        { inherit (snack) snack-exe snack-lib; }
      )
    ];
    config = {};
  }
