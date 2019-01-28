with { fetch = import ./fetch.nix; };
import fetch.nixpkgs
  { overlays =
      [ (self: super:
          {
            snack-exe = (import fetch.snack).snack-exe;
            snack-lib = (import fetch.snack).snack-lib;
          }
        )
      ] ;
    config = { } ;
  }
