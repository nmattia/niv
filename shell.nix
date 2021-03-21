{ pkgs ? import ./nix { } }:
(import ./default.nix { inherit pkgs; }).niv-devshell
