{ sources ? import ./nix/sources.nix
, pkgs ? import ./nix { inherit sources; }
}:

let
  sourceByRegex = name: src: regexes:
    builtins.path {
      filter = path: type:
        let
          relPath = pkgs.lib.removePrefix (toString src + "/") (toString path);
          accept = pkgs.lib.any (re: builtins.match re relPath != null) regexes;
        in
        accept;
      inherit name;
      path = src;
    };

  niv-source = sourceByRegex "niv" ./. [
    "^package.yaml$"
    "^README.md$"
    "^LICENSE$"
    "^app$"
    "^app.*.hs$"
    "^src$"
    "^src/Data$"
    "^src/Data/Aeson$"
    "^src/Data/HashMap$"
    "^src/Data/HashMap/Strict$"
    "^src/Data/Text$"
    "^src/Niv$"
    "^src/Niv/Git$"
    "^src/Niv/GitHub$"
    "^src/Niv/Local$"
    "^src/Niv/Sources$"
    "^src/Niv/Update$"
    "^src.*.hs$"
    "^README.md$"
    "^nix$"
    "^nix.sources.nix$"
  ];

in
rec
{
  niv = pkgs.haskellPackages.callPackage
    (
      { aeson
      , aeson-pretty
      , ansi-terminal
      , base
      , bytestring
      , directory
      , file-embed
      , filepath
      , hashable
      , http-conduit
      , mtl
      , optparse-applicative
      , process
      , profunctors
      , pureMD5
      , string-qq
      , tasty
      , tasty-hunit
      , text
      , unliftio
      , unordered-containers
      , mkDerivation
      }:
      mkDerivation {
        pname = "niv";
        version = "0.22";
        src = niv-source;
        isLibrary = true;
        isExecutable = true;
        libraryToolDepends = [ pkgs.haskellPackages.hpack ];
        prePatch = "hpack";
        libraryHaskellDepends = [
          aeson
          aeson-pretty
          ansi-terminal
          base
          bytestring
          directory
          file-embed
          filepath
          hashable
          http-conduit
          mtl
          optparse-applicative
          process
          profunctors
          pureMD5
          string-qq
          tasty
          tasty-hunit
          text
          unliftio
          unordered-containers
        ];
        executableHaskellDepends = [ ];
        testHaskellDepends = [ ];
        description = "Easy dependency management for Nix projects";
        homepage = "https://github.com/nmattia/niv#readme";
        license = pkgs.lib.licenses.mit;
      }
    )
    { };

  # cabal-friendly sdist
  niv-sdist = pkgs.haskell.lib.sdistTarball niv;

  tests-github = pkgs.callPackage ./tests/github { inherit niv; };
  tests-git = pkgs.callPackage ./tests/git { inherit niv; };
  tests-eval = pkgs.callPackage ./tests/eval { };

  fmt-check =
    pkgs.stdenv.mkDerivation
      {
        name = "fmt-check";
        buildInputs = [ pkgs.ormolu pkgs.glibcLocales ];
        src = niv-source;
        phases = [ "unpackPhase" "checkPhase" ];
        LANG = "en_US.UTF-8";
        checkPhase = ''
          cp ${./script/fmt} ./fmt
          patchShebangs ./fmt
          chmod +x fmt
          bash fmt -c
          touch $out
        '';
        doCheck = true;
      };

  readme = pkgs.runCommand "README.md" { nativeBuildInputs = [ niv pkgs.moreutils ]; }
    ''
      cp ${./README.tpl.md} $out
      chmod +w $out

      sed "/replace_niv_help/r"<(niv --help) $out | sponge $out
      sed "/replace_niv_help/d" $out | sponge $out

      sed "/replace_niv_add_help/r"<(niv add --help) $out | sponge $out
      sed "/replace_niv_add_help/d" $out | sponge $out

      sed "/replace_niv_update_help/r"<(niv update --help) $out| sponge $out
      sed "/replace_niv_update_help/d" $out | sponge $out

      sed "/replace_niv_modify_help/r"<(niv modify --help) $out | sponge $out
      sed "/replace_niv_modify_help/d" $out | sponge $out

      sed "/replace_niv_drop_help/r"<(niv drop --help) $out | sponge $out
      sed "/replace_niv_drop_help/d" $out | sponge $out

      sed "/replace_niv_init_help/r"<(niv init --help) $out | sponge $out
      sed "/replace_niv_init_help/d" $out | sponge $out

      sed "/replace_niv_show_help/r"<(niv show --help) $out | sponge $out
      sed "/replace_niv_show_help/d" $out | sponge $out
    '';

  readme-test = pkgs.runCommand "README-test" { }
    ''
      err() {
        echo
        echo -e "\e[31mERR\e[0m: README.md out of date"
        echo -e "please run \e[1m./script/gen\e[0m"
        echo
        exit 1
      }

      diff ${./README.md} ${readme} && echo dummy > $out || err ;
    '';
}
