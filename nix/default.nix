{ sources ? import ./sources.nix }:
let
  ormoluCompiler = "ghc865";
in
import sources.nixpkgs {
  overlays = [
    (_: pkgs: { inherit sources; })
    (_: pkgs: { nixpkgs-fmt = import pkgs.sources.nixpkgs-fmt {}; })
    (_: pkgs: { termtosvg = pkgs.callPackage ./termtosvg.nix {}; })
    (import ./haskell-overlay.nix { inherit ormoluCompiler; })
    (_: pkgs: { ormolu = pkgs.haskell.packages."${ormoluCompiler}".ormolu; })
  ];
  config = {};
}
