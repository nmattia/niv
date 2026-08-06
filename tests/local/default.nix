{ pkgs, niv }:

{
  local =
    pkgs.runCommand "local-test"
      { nativeBuildInputs = [ niv pkgs.nix pkgs.jq ]; }
      (

        ''
      # don't use /nix/store (even evaluation adds files to the store)
      # https://github.com/NixOS/nix/issues/3258
      export NIX_REMOTE="local?root=$TMPDIR/local-test-store"
      export NIX_STATE_DIR=$TMPDIR

      export HOME="$TMPDIR/homeless"

      # custom nix.conf
      export NIX_USER_CONF_FILES=$(mktemp)
      echo 'extra-experimental-features = nix-command flakes' >> "$NIX_USER_CONF_FILES"

      # create a dir

      localdir="my-dir"
      mkdir -p "$localdir"
      pushd $localdir > /dev/null
      echo hello > file
      echo world >> file
      popd > /dev/null

      # then we niv add the dir containing the files

      nivdir=$(mktemp -d)
      pushd $nivdir > /dev/null
      mkdir -p nix
      echo "{}" > nix/sources.json
      niv init --latest
      niv add my-dir --attribute path=$localdir --type local

      nivdir=$(nix eval --json --impure --expr '(import ./nix/sources.nix).my-dir.path' | jq -r)
      if [ ! "$localdir" = "$nivdir" ]; then
        echo "Mismatched dirs: $localdir != $nivdir"
        exit 42
      fi

      popd > /dev/null

      touch $out
        ''
      );
}
