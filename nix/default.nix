{ sources ? import ./sources.nix }:
import sources.nixpkgs {
  overlays = [
    (_: pkgs: { inherit sources; })
    (_: pkgs: {
      nixpkgs-fmt =
        let naersk = pkgs.callPackage pkgs.sources.naersk { };
        in naersk.buildPackage pkgs.sources.nixpkgs-fmt;
    })
    (_: pkgs: { termtosvg = pkgs.callPackage ./termtosvg.nix { }; })
  ];
  config = { };
}
