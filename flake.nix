{
  description = "Easy dependency management for Nix projects.";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs?ref=nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:

    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}";

        haskellPackages = pkgs.haskellPackages.override {
                overrides = self: super: {
                    # 0.19+ is required for niv to build and at the time of writing nixpkgs defaults to 0.18
                    optparse-applicative = self.optparse-applicative_0_19_0_0;
                };
            };

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

        niv = haskellPackages.callPackage
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

        # golden tests for TUI. The idle-time-limit is taken to be slightly shorter than the pauses
        # in the debug output, so each pause creates a new frame.
        golden = pkgs.runCommand "golden" { nativeBuildInputs = [ niv pkgs.asciinema pkgs.asciinema-agg ]; }
          ''
              mkdir -p $out
              niv --help # some systems (macOS) are a bit slow to run a new binary
              # pairs of job name & window dimensions
              goldens=("job-hello-world,16x5" "job-multi,24x6" "job-note-multiline,32x8" "job-every-admonition,24x10")
              font_dir=${pkgs.nerd-fonts.jetbrains-mono}/share/fonts/truetype/NerdFonts/JetBrainsMono
              font_family="JetBrainsMono Nerd Font"
              for cfg in "''${goldens[@]}"; do
                  IFS=',' read -r golden dims <<<"$cfg"
                  asciinema record --window-size "$dims" --command "niv debug $golden" "$golden.cast"
                  agg --font-dir "$font_dir" --text-font-family "$font_family" --idle-time-limit 0.5 "$golden.cast" "$out/$golden.gif"
              done
          '';

        readme = pkgs.runCommand "README.md" { nativeBuildInputs = [ niv pkgs.moreutils ]; }
          ''
            mkdir -p $out
            readme="$out/README.md"
            cat ${./README.tpl.md} > "$readme"

            sed "/replace_niv_help/r"<(niv --help) $readme | sponge $readme
            sed "/replace_niv_help/d" $readme | sponge $readme

            sed "/replace_niv_add_help/r"<(niv add --help) $readme | sponge $readme
            sed "/replace_niv_add_help/d" $readme | sponge $readme

            sed "/replace_niv_update_help/r"<(niv update --help) $readme| sponge $readme
            sed "/replace_niv_update_help/d" $readme | sponge $readme

            sed "/replace_niv_rename_help/r"<(niv rename --help) $readme| sponge $readme
            sed "/replace_niv_rename_help/d" $readme | sponge $readme

            sed "/replace_niv_modify_help/r"<(niv modify --help) $readme | sponge $readme
            sed "/replace_niv_modify_help/d" $readme | sponge $readme

            sed "/replace_niv_drop_help/r"<(niv drop --help) $readme | sponge $readme
            sed "/replace_niv_drop_help/d" $readme | sponge $readme

            sed "/replace_niv_init_help/r"<(niv init --help) $readme | sponge $readme
            sed "/replace_niv_init_help/d" $readme | sponge $readme

            sed "/replace_niv_show_help/r"<(niv show --help) $readme | sponge $readme
            sed "/replace_niv_show_help/d" $readme | sponge $readme
          '';
      in
      {
        packages = {
          inherit niv niv-sdist readme golden;
        };

        checks = import ./tests { inherit system pkgs niv; };

      }
    );
}
