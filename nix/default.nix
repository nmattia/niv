{ sources ? import ./sources.nix {} }:
import sources.nixpkgs
  { overlays =
    [ (_: _: { inherit sources; })
      (_: super: { termtosvg = super.callPackage ./termtosvg.nix {}; })
    ];
    config = {};
  }
