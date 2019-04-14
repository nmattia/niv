{ sources ? import ./sources.nix }:
import sources.nixpkgs
  { overlays =
    [ (_: pkgs:
        { termtosvg = pkgs.callPackage ./termtosvg.nix {}; }
      )
    ];
    config = {};
  }
