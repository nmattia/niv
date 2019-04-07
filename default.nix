{ pkgs ? import ./nix {} }:
with rec
{ niv-source = pkgs.lib.sourceByRegex ./.
    [ "^package.yaml$"
      "^app.*$"
      "^README.md$"
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
}
