{ sources ? import ./sources.nix, inNixShell ? null /* nix-shell sometimes adds this */ }:
import sources.nixpkgs {
  overlays = [
    (_: pkgs: { inherit sources; })
    (_: pkgs: { termtosvg = pkgs.callPackage ./termtosvg.nix {}; })
  ];
  config = {};
}
