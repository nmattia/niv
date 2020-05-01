{ ormoluCompiler }: self: super:

let
  ormolu = import super.sources.ormolu { pkgs = self; inherit ormoluCompiler; };
in
{
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      "${ormolu.ormoluCompiler}" = super.haskell.packages.${ormolu.ormoluCompiler}.override {
        overrides = ormolu.ormoluOverlay;
      };
    };
  };
}
