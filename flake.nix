{
  description = "Easy dependency management for Nix projects.";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs?ref=nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:

    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}";

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
          "^niv.cabal$"
          "^README.md$" # the README is not required for the build but is required for the sdist
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
          "^nix$"
          "^nix.sources.nix$"
        ];

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
              version = "0.2.22";
              src = niv-source;
              isLibrary = true;
              isExecutable = true;
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

        readme = pkgs.runCommand "README.md" { nativeBuildInputs = [ niv pkgs.moreutils ]; }
          ''
            cp ${./README.tpl.md} $out
            chmod +w $out

            sed "/replace_niv_help/r"<(niv --help) $out | sponge $out
            sed "/replace_niv_help/d" $out | sponge $out

            sed "/replace_niv_add_help/r"<(niv add --help) $out | sponge $out $out
            sed "/replace_niv_add_help/d" $out | sponge $out

            sed "/replace_niv_update_help/r"<(niv update --help) $out| sponge $out $out
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
      in
      {
        packages = {
          inherit niv niv-sdist readme;
        };

        checks = import ./tests { inherit system pkgs niv; };

      }
    );
}
