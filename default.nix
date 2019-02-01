with { pkgs = import ./nix {}; };
{ inherit (pkgs) niv readme readme-test ; }
