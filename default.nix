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

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {

      niv =
        pkgs.haskell.lib.justStaticExecutables (
          pkgs.haskell.lib.failOnAllWarnings (
            pkgs.haskell.lib.disableExecutableProfiling (
              pkgs.haskell.lib.disableLibraryProfiling (
                pkgs.haskellPackages.generateOptparseApplicativeCompletions [ "niv" ] (
                  (pkgs.callPackage ./foo { haskellPackages = self; }).buildPackage { root = ./.; src = niv-source; }
                )
              )
            )
          )
        );
    };
  };

  inherit (haskellPackages) niv;

  niv-sdist = pkgs.haskell.lib.sdistTarball niv;

  niv-cabal-upload =
    let
      niv-version = niv.version;
    in
    pkgs.writeShellScript "cabal-upload"
      ''
        cabal upload "$@" "${niv-sdist}/niv-${niv-version}.tar.gz"
      '';

  # WARNING: extremely disgusting hack below.
  #
  # <rant>
  #   I was trying to fix this issue: https://github.com/nmattia/niv/issues/109
  #   Basically, --help should also show niv's version. As usual when trying to
  #   lookup the documentation for `Paths_` I google "cabal Paths_ module", end
  #   up here:
  #   https://stackoverflow.com/questions/21588500/haskell-cabal-package-cant-find-paths-module,
  #   click this link:
  #   https://downloads.haskell.org/~ghc/7.0.3/docs/html/Cabal/authors.html#paths-module,
  #   and then try to find my way to the latest doc.
  #
  #   (╯°□°)╯︵ ┻━┻
  #
  #   But now that we're using cabal Paths_, the `ghci` `repl` function won't
  #   work! So I wonder: stack or cabal? Well, stack, it's been a while! But
  #   _of course_ there's no way to easily tell stack to use the system GHC!
  #   Even if there's no `stack.yaml`, stack will infer "some" LTS and
  #   `--system-ghc` will be discarded because the GHC versions don't match!!
  #
  #   (╯°□°)╯︵ ┻━┻
  #
  #   So I give good old `cabal repl` a try and, (not much of a) surprise, it
  #   complains that it can't start because niv contains more than one cabal
  #   component.
  #
  #   (╯°□°)╯︵ ┻━┻
  #
  #   WTF, there's now repl, new-repl, v1-repl, v2-repl. WHAT?
  #
  #   (˚Õ˚)ر ~~~~╚╩╩╝
  #
  #   So I try `cabal new-repl`, and it doesn't load _any_ main function
  #   (without having to actually write `main = Niv.Test.test` or doing weird
  #   stuff to load a `Main` module).
  #
  #   (╯°□°)╯︵ ┻━┻
  #
  #   And there's no `cabal new-repl -main-is ...`
  #
  #   (╯°□°)╯︵ ┻━┻
  #
  #   The workaround here:
  #   https://github.com/haskell/cabal/issues/5374#issuecomment-410431619
  #   suggests using -ghci-script=foo where foo specifies `main = bar`, but, as
  #   pointed out, the ghci script runs too early (and that's after having
  #   tried --ghci-options which doesn't work; must use --repl-options).
  #
  #   (╯°□°)╯︵ ┻━┻ (˚Õ˚)ر ~~~~╚╩╩╝
  #
  #   But there's hope! The `cabal new-repl` doc says you can load a
  #   "component". So I try with `cabal new-repl niv-test`. FIRST it builds
  #   everything (actually generates `.o`s and stuff) and then loads the
  #   _wrong_ module.  At this point I can't help thinking that a monkey
  #   (bonobo, baboon, pick any) will still manage to pick the correct main 50%
  #   of the time. I don't want to jump to conclusion: I only gave cabal one
  #   chance, so I'm not sure how exactly it scores on the "pick the correct
  #   main" game.
  #
  #   (゜-゜)
  #
  #   Well,
  #     => rm -rf ~/.stack
  #     => rm -rf ~/.cabal
  #     => rm -rf dist
  #     => rm -rf dist-newstyle
  #
  # </rant>
  #
  # In order to make `Paths_niv(version)` available in `ghci`, we parse the
  # version from `package.yaml` and create a dummy module that we inject in the
  # `ghci` command.
  niv-devshell = haskellPackages.shellFor {
    packages = ps: [ ps.niv ];
    buildInputs = [ pkgs.ormolu pkgs.glibcLocales ];
    shellHook = ''
      repl_for() {
        haskell_version=$(jq <./package.yaml -cMr '.version' | sed 's/\./,/g')

        paths_niv=$(mktemp -d)/Paths_niv.hs

        echo "module Paths_niv where" >> $paths_niv
        echo "import qualified Data.Version" >> $paths_niv
        echo "version :: Data.Version.Version" >> $paths_niv
        echo "version = Data.Version.Version [$haskell_version] []" >> $paths_niv

        niv_main=""

        shopt -s globstar
        ghci -clear-package-db -global-package-db -Wall app/$1.hs src/**/*.hs $paths_niv
      }

      repl() {
        repl_for NivTest
      }

      repl_niv() {
        repl_for Niv
      }

      echo "To start a REPL for the test suite, run:"
      echo "  > repl"
      echo "  > :main"
      echo "  (tests run)"
      echo
      echo "To start a REPL session emulating the niv executable, run:"
      echo "  > repl_niv"
      echo "  > :main --help ..."
      echo "  NIV - Version manager for Nix projects"
      echo "  ..."
    '';
  };
in
rec
{
  inherit niv niv-sdist niv-source niv-devshell niv-cabal-upload;

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

      echo done
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

  niv-svg-test = pkgs.runCommand "niv-svg-test" { }
    ''
      # XXX: This test means that the svg needs to be regenerated
      # by hand on (virtually) every commit.
      # TODO: figure out a nicer way
      touch $out
      exit 0

      err() {
        echo
        echo -e "\e[31mERR\e[0m: niv.svg out of date"
        echo -e "please run \e[1m./script/gen\e[0m"
        echo
        exit 1
      }

      expected_hash=$(${pkgs.nix}/bin/nix-hash ${niv-svg-gen})
      actual_hash=$(grep -oP 'id="\K[^"]+' ${./niv.svg} -m 1)

      echo "expected $expected_hash"
      echo "actual   $actual_hash"

      [ $expected_hash == $actual_hash ] && echo dymmy > $out || err
    '';

  # TODO: use nivForTest for this one
  niv-svg-cmds = pkgs.writeScript "niv-svg-cmds"
    ''
      #!${pkgs.stdenv.shell}
      set -euo pipefail
      echo '$ niv init'
      niv init
      echo
      echo '$ niv add stedolan/jq'
      niv add stedolan/jq
    '';

  niv-svg-gen = pkgs.writeScript "niv-svg-gen"
    ''
      #!${pkgs.stdenv.shell}
      set -euo pipefail
      export PATH=${haskellPackages.niv}/bin:${pkgs.nix}/bin:$PATH

      pushd $(mktemp -d)
      ${pkgs.termtosvg}/bin/termtosvg \
          -g 82x26 -M 2000 -m 2000 -t gjm8 \
          -c '${niv-svg-cmds}' $PWD/niv.svg

      echo done rendering
      popd
    '';
}
