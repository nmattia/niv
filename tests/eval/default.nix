{ pkgs ? import <nixpkgs> {}
}:


let
  mkTest = name: text:
    {
      ${name} =
        pkgs.runCommand name { nativeBuildInputs = [ pkgs.jq pkgs.nix pkgs.moreutils ]; }
          ''
            # for nix to run smoothly in multi-user install
            # https://github.com/NixOS/nix/issues/3258
            # https://github.com/cachix/install-nix-action/issues/16
            export NIX_STATE_DIR="$TMPDIR"
            export NIX_LOG_DIR="$TMPDIR"

            cp ${ ../../nix/sources.nix} sources.nix
            echo '{}' > sources.json

            update_sources() {
              cat sources.json | jq -cMe "$@" | sponge sources.json
            }

            eval_outPath() {
              nix eval --raw '(let sources = import ./sources.nix; in sources.'"$1"'.outPath)'
            }

            eq() {
              if ! [ "$1" == "$2" ]; then
                echo "expected"
                echo "  '$1' == '$2'"
                exit 1
              fi
            }

            ${text}

            touch "$out"
          '';
    };
in

mkTest "simple-eval" ''

        update_sources '.foo = { type: "tarball", url: "foo", sha256: "whocares" }'
        update_sources '."ba-r" = { type: "tarball", url: "foo", sha256: "whocares" }'
        update_sources '."ba z" = { type: "tarball", url: "foo", sha256: "whocares" }'

        res="$(NIV_OVERRIDE_foo="hello" eval_outPath "foo")"
        eq "$res" "hello"

        res="$(NIV_OVERRIDE_ba_r="hello" eval_outPath "ba-r")"
        eq "$res" "hello"

        res="$(NIV_OVERRIDE_ba_z="hello" eval_outPath '"ba z"')"
        eq "$res" "hello"

  '' // mkTest "sources-json-elsewhere"
  ''
    update_sources '.foo = { type: "tarball", url: "foo", sha256: "whocares" }'

    mkdir other
    mv sources.json other

    # Here we test that sources.nix can be imported even if there is no
    # sources.json in the same directory
    eval_outPath() {
      nix eval --raw '(let sources = import ./sources.nix { sourcesFile = ./other/sources.json; } ; in sources.'"$1"'.outPath)'
    }

    res="$(NIV_OVERRIDE_foo="hello" eval_outPath "foo")"
    eq "$res" "hello"
  ''
