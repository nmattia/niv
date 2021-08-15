{ haskellPackages
, stdenv
, lib
}:

{
  buildPackage =
    attrs:
      let
        src = if !lib.isDerivation attrs && lib.isAttrs attrs then attrs.src else attrs;
        root = if !lib.isDerivation attrs && lib.isAttrs attrs then attrs.root else attrs;
        nubdeps = ds: lib.lists.sort (x: y: x < y) (
          lib.unique (
            map (d: lib.head (lib.splitString " " d)) ds
          )
        );
        spec = builtins.fromJSON (builtins.readFile (root + "/package.yaml"));
        commonDeps = spec.dependencies;

        libraryExtraDeps =
          lib.optionals
            (spec ? library && spec.library ? dependencies)
            spec.library.dependencies;
        libraryDeps = nubdeps (commonDeps ++ libraryExtraDeps);

        exeExtraDeps = lib.optionals (spec ? executables) (
          lib.concatMap
            (
              exe: lib.optionals
                (exe ? dependencies)
                exe.dependencies
            )
            (builtins.attrValues spec.executables)
        );
        exeDeps =
          nubdeps
            (
              builtins.filter (x: x != spec.name)
                (commonDeps ++ exeExtraDeps)
            );

        testExtraDeps = lib.optionals (spec ? tests) (
          lib.concatMap
            (
              test: lib.optionals
                (test ? dependencies)
                test.dependencies
            )
            (builtins.attrValues spec.tests)
        );
        testDeps = nubdeps (builtins.filter (x: x != spec.name) (commonDeps ++ testExtraDeps));

        depsFor = depType:
          map (
            d:
              if ! builtins.hasAttr d haskellPackages
              then throw "haskellPackages does not contain dependency '${d}' needed for '${depType}'"
              else
                haskellPackages.${d}
          );

      in
        haskellPackages.callPackage (
          { mkDerivation }:
            mkDerivation {
              pname = spec.name;
              version = spec.version;
              inherit src;
              isLibrary = builtins.hasAttr "library" spec;
              isExecutable = builtins.hasAttr "executables" spec;
              enableSeparateDataOutput = true;
              libraryHaskellDepends = depsFor "libraryHaskellDepends" libraryDeps;
              libraryToolDepends = [ haskellPackages.hpack ];
              executableHaskellDepends = depsFor "executableHaskellDepends" exeDeps;
              testHaskellDepends = depsFor "testHaskellDepends" testDeps;
              prePatch = "hpack";
              homepage = "https://github.com/${spec.github}#readme";
              description = spec.synopsis;
              license =
                if builtins.hasAttr "license" spec && spec.license == "MIT"
                then lib.licenses.mit
                else throw "Don't know how to handle license: ${builtins.toJSON spec.license}";
            }
        ) {};
}
