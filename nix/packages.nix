{ writeText
, runCommand
, lib
, snack-lib
}:
rec
{ niv = snack-lib.executable ../package.yaml;
  readme = writeText "README.md"
    (with
      { template = builtins.readFile ../README.tpl.md;
        niv_help = builtins.readFile
          (runCommand "niv_help" { buildInputs = [ niv ]; }
            "niv --help > $out"
          );
        niv_add_help = builtins.readFile
          (runCommand "niv_add_help" { buildInputs = [ niv ]; }
            "niv add --help > $out"
          );
        niv_update_help = builtins.readFile
          (runCommand "niv_update_help" { buildInputs = [ niv ]; }
            "niv update --help > $out"
          );
        niv_drop_help = builtins.readFile
          (runCommand "niv_drop_help" { buildInputs = [ niv ]; }
            "niv drop --help > $out"
          );
      };
    lib.replaceStrings
      [
        "replace_niv_help"
        "replace_niv_add_help"
        "replace_niv_update_help"
        "replace_niv_drop_help"
      ]
      [ niv_help niv_add_help niv_update_help niv_drop_help ]
      template
    );
  readme-test = runCommand "README-test" {}
    "diff ${../README.md} ${readme} && echo dummy > $out";
}
