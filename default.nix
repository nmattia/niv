{ pkgs ? import ./nix {} }:
with rec
{ files = pkgs.callPackage ./nix/files.nix {};
  niv-source = files.sourceByRegex "niv" ./.
    [ "^package.yaml$"
      "^app.*$"
      "^README.md$"
      "^nix.sources.nix$"
    ];
  haskellPackages = pkgs.haskellPackages.override
    { overrides = _: haskellPackages:
        { niv = haskellPackages.callCabal2nix "niv" niv-source {}; };
    };

  niv-devshell = haskellPackages.shellFor
    { packages = (ps: [ ps.niv ]);
      shellHook =
        ''
          repl() {
            ghci app/Niv.hs
          }

          echo "To start a REPL session, run:"
          echo "  > repl"
        '';
    };

};
rec
{ inherit niv-source niv-devshell;
  inherit (haskellPackages) niv;
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
      err() {
        echo
        echo -e "\e[31mERR\e[0m: niv.svg out of date"
        echo -e "please run \e[1m./script/gen\e[0m"
        echo
        exit 1
      }

      expected_hash=$(${pkgs.nix}/bin/nix-hash ${niv-svg-gen})
      actual_hash=$(grep -oP 'id="\K[^"]+' ${./site/niv.svg} -m 1)

      echo expected $expected_hash
      echo actuall $actual_hash

      [ $expected_hash == $actual_hash ] && echo dymmy > $out || err
    '';

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
