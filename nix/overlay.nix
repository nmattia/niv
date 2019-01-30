{ sources ? import ./fetch.nix }:
[
  (self: super:
    {
      snack-exe = (import sources.snack).snack-exe;
      snack-lib = (import sources.snack).snack-lib;
    }
  )

  (self: super:
    { niv = super.snack-lib.inferHPackBuild ../package.yaml ; }
  )
]
