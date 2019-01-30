{ sources ? import ./fetch.nix }:
[
  (self: super:
    {
      snack-exe = (import sources.snack).snack-exe;
      snack-lib = (import sources.snack).snack-lib;
    }
  )

  (self: super:
    { niv = super.snack-lib.executable ../package.yaml ; }
  )

  (self: super:
    { readme = self.writeText "README.md"
        (with
          { template = builtins.readFile ../README.tpl.md;
            niv_help = builtins.readFile
              (self.runCommand "niv_help" { buildInputs = [ self.niv ]; }
                "niv --help > $out"
              );
            niv_add_help = builtins.readFile
              (self.runCommand "niv_add_help" { buildInputs = [ self.niv ]; }
                "niv add --help > $out"
              );
            niv_update_help = builtins.readFile
              (self.runCommand "niv_update_help" { buildInputs = [ self.niv ]; }
                "niv update --help > $out"
              );
            niv_drop_help = builtins.readFile
              (self.runCommand "niv_drop_help" { buildInputs = [ self.niv ]; }
                "niv drop --help > $out"
              );
          };
        self.lib.replaceStrings
          [
            "replace_niv_help"
            "replace_niv_add_help"
            "replace_niv_update_help"
            "replace_niv_drop_help"
          ]
          [ niv_help niv_add_help niv_update_help niv_drop_help ]
          template
        );
    }
  )

]
