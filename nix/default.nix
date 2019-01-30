with { fetch = import ./fetch.nix; };
import fetch.nixpkgs
  { overlays = import ./overlay.nix { sources = fetch; } ; config = {}; }
