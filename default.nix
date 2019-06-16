{ sources ? import ./nix/sources.nix
, pkgs ? import ./nix { inherit sources; }
}:

with rec
{ files = pkgs.callPackage ./nix/files.nix {};
  gitignoreSource = (pkgs.callPackage sources.gitignore {}).gitignoreSource;
  niv-source = gitignoreSource ./.;
  haskellPackages = pkgs.haskellPackages.override
    { overrides = _: haskellPackages:
        { niv =
            pkgs.haskell.lib.disableExecutableProfiling (
            pkgs.haskell.lib.disableLibraryProfiling (
            pkgs.haskell.lib.generateOptparseApplicativeCompletion "niv" (
            haskellPackages.callCabal2nix "niv" niv-source {})));
        };
    };

  niv = haskellPackages.niv;

  niv-sdist = pkgs.haskell.lib.sdistTarball niv;

  niv-cabal-upload =
    with
      {  niv-version = niv.version;
      };
    pkgs.writeScript "cabal-upload"
    ''
      cabal upload "$@" "${niv-sdist}/niv-${niv-version}.tar.gz"
    '';

  niv-devshell = haskellPackages.shellFor
    { packages = (ps: [ ps.niv ]);
      shellHook =
        ''
          repl() {
            shopt -s globstar
            ghci -clear-package-db -global-package-db -Wall app/NivTest.hs src/**/*.hs
          }

          repl_niv() {
            shopt -s globstar
            ghci -Wall app/Niv.hs src/**/*.hs
          }

          echo "To start a REPL session for the test suite, run:"
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

};
rec
{ inherit niv niv-sdist niv-source niv-devshell niv-cabal-upload;

  tests = pkgs.callPackage ./tests { inherit niv; };

  niv-test = pkgs.runCommand "niv-test" { buildInputs = [ niv ] ; }
    "niv-test && touch $out";

  readme = pkgs.writeText "README.md"
    (with
      { template = builtins.readFile ./README.tpl.md;
        niv_help = builtins.readFile
          (pkgs.runCommand "niv_help" { buildInputs = [ niv ]; }
            "niv --help > $out"
          );
        niv_cmd_help = cmd: builtins.readFile
          (pkgs.runCommand "niv_${cmd}_help" { buildInputs = [ niv ]; }
            "niv ${cmd} --help > $out"
          );
        cmds = [ "add" "update" "drop" "init" "show" ];
      };
    pkgs.lib.replaceStrings
      ([ "replace_niv_help" ] ++ (map (cmd: "replace_niv_${cmd}_help") cmds))
      ([ niv_help ] ++ (map niv_cmd_help cmds))
      template
    );
  readme-test = pkgs.runCommand "README-test" {}
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

  niv-svg-test = pkgs.runCommand "niv-svg-test" {}
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
      actual_hash=$(grep -oP 'id="\K[^"]+' ${./site/niv.svg} -m 1)

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

      hash=$(${pkgs.nix}/bin/nix-hash ''${BASH_SOURCE[0]})
      pushd $(mktemp -d)
      (tail -f /dev/null || true) | ${pkgs.termtosvg}/bin/termtosvg \
          -g 82x26 -M 1500 -m 1500 -t window_frame \
          -c '${niv-svg-cmds}' niv.svg
      ${pkgs.gnused}/bin/sed -i "0,/terminal/{s/terminal/$hash/}" niv.svg
      niv_svg=$(realpath niv.svg)
      popd

      cp $niv_svg site/niv.svg
      '';

}
