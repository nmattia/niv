{ sources ? import ./sources.nix }:
import sources.nixpkgs {
  overlays = [
    (_: pkgs: { inherit sources; })
    (_: pkgs: { nixpkgs-fmt = import pkgs.sources.nixpkgs-fmt {}; })
    (_: pkgs: { termtosvg = pkgs.callPackage ./termtosvg.nix {}; })
  ];
  config = {};
}
