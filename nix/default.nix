{ sources ? import ./sources.nix }:
import sources.nixpkgs
  { overlays =
    [
      # Snack
      (self: super:
        {
          snack-exe = (import sources.snack).snack-exe;
          snack-lib = (import sources.snack).snack-lib;
        }
      )
    ];
    config = {};
  }
