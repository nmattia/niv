{ system, pkgs, niv }:
let

  # takes an attrset of derivations as 'tests' and returns one derivation
  collectResults = name: tests:
    let
      # aggregate all attributes into a string
      flatten = attrs:
        pkgs.lib.foldlAttrs (acc: key: value: acc + "${key}: ${value}; ") "" attrs;
    in
    pkgs.runCommand name { TESTS = flatten tests; } ''
      echo tests successful
      touch $out
    '';

  # tests themselves
  eval = collectResults "eval-tests" (import ./eval { inherit pkgs niv; });
  github = collectResults "github-tests" (import ./github { inherit pkgs niv; });
  git = collectResults "git-tests" (import ./git { inherit pkgs niv; });

in

{ inherit eval github git; }
