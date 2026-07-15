{ pkgs, ... }:

let
  mkTest = name: text:
    {
      ${name} =
        pkgs.runCommand name { nativeBuildInputs = [ pkgs.jq pkgs.nix pkgs.moreutils ]; }
          ''
            echo eval test "${name}" running...
            export NIX_REMOTE="local?root=$TMPDIR/git-test-store"
            export NIX_STATE_DIR=$TMPDIR

            export HOME="$TMPDIR/homeless"
            export NIX_USER_CONF_FILES=$(mktemp)
            # allow 'nix foo' commands
            echo 'extra-experimental-features = nix-command flakes' >> "$NIX_USER_CONF_FILES"
            # disable substituters since we run in the sandbox (some commands try to fetch nix-cache-info and fail)
            echo 'substituters = ' >> "$NIX_USER_CONF_FILES"

            cp ${ ../../nix/sources.nix} sources.nix
            echo '{}' > sources.json

            update_sources() {
              cat sources.json | jq -cMe "$@" | sponge sources.json
            }

            eval_outPath() {
              nix eval --raw --impure --expr '(let sources = import ./sources.nix; in toString sources.'"$1"'.outPath)'
            }

            eq() {
              if ! [ "$1" == "$2" ]; then
                echo "expected"
                echo "  '$1' == '$2'"
                exit 1
              fi
            }

            ${text}

            echo eval test "${name}" passed
            echo OK > "$out"
          '';
    };
in

mkTest "niv-override-eval" ''

        update_sources '.foo = { type: "tarball", url: "foo", sha256: "whocares" }'
        update_sources '."ba-r" = { type: "tarball", url: "foo", sha256: "whocares" }'
        update_sources '."ba z" = { type: "tarball", url: "foo", sha256: "whocares" }'

        res="$(NIV_OVERRIDE_foo="hello" eval_outPath "foo")"
        eq "$res" "$PWD/hello"

        res="$(NIV_OVERRIDE_ba_r="hello" eval_outPath "ba-r")"
        eq "$res" "$PWD/hello"

        res="$(NIV_OVERRIDE_ba_z="hello" eval_outPath '"ba z"')"
        eq "$res" "$PWD/hello"

  '' // mkTest "sources-json-elsewhere"
  ''
    update_sources '.foo = { type: "tarball", url: "foo", sha256: "whocares" }'

    mkdir other
    mv sources.json other

    # Here we test that sources.nix can be imported even if there is no
    # sources.json in the same directory
    eval_outPath() {
      nix eval --raw --impure --expr '(let sources = import ./sources.nix { sourcesFile = ./other/sources.json; } ; in toString sources.'"$1"'.outPath)'
    }

    res="$(NIV_OVERRIDE_foo="hello" eval_outPath "foo")"
    eq "$res" "$PWD/hello"
  ''

  // mkTest "sanitize-source-name"
  ''
    file=$(mktemp -d)/foo%%.bar
    touch "$file"
    sha=$(nix-hash --type sha256 --flat $file)
    url="file://$file"

    update_sources '.foo = { type: "file", url: $url, sha256: $sha}' --arg url "$url" --arg sha "$sha"

    # we don't need to check the result, we just make sure this evaluates
    eval_outPath "foo" >/dev/null
  ''
