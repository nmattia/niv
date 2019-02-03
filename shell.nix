with { pkgs = import ./nix {}; };
pkgs.mkShell
  { buildInputs = [ pkgs.snack-exe ];
  }
