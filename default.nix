{ pkgs ? import ./nix {} }:
pkgs.callPackage ./nix/packages.nix {}
