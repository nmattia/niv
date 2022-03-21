{ pkgs, niv }:

# TODO: this doesn' test anything meaningful yet because "niv git PACKAGE"
# doesn't parse yet
pkgs.runCommand "git-test"
  { nativeBuildInputs = [ pkgs.git niv pkgs.nix pkgs.jq ]; }
  (

    # make sure the tests run smoothly in multi-user install
    # https://github.com/NixOS/nix/issues/3258
    ''
      export NIX_STATE_DIR=$TMPDIR
      export NIX_LOG_DIR=$TMPDIR
      export HOME=$TMPDIR
    '' + # First we create a dummy git repo with one commit on master, and one commit
    # on "branch".
    ''
      gitdir=$(mktemp -d)
      pushd $gitdir > /dev/null
      git init .
      echo hello > file
      git config user.email "niv@foo.bar"
      git config user.name "Niv Niverson"
      git add file
      git commit -m "Initial commit"
      gitrev=$(git rev-parse HEAD)

      git checkout -b branch
      echo world >> file
      git add file
      git commit -m "second commit"
      gitrev2=$(git rev-parse HEAD)

      # reset to master as "default branch"
      git checkout master
      popd > /dev/null
    '' + # Then we `niv add` that repo and check some properties, like the revision
    # and revCount, to make sure it was imported properly, and that sources.nix
    # does what it's supposed to do.
    ''
      nivdir=$(mktemp -d)
      pushd $nivdir > /dev/null
      mkdir -p nix
      echo "{}" > nix/sources.json
      niv init --latest
      niv add git -n my-git-repo --repo file://$gitdir
      nivrev=$(nix --extra-experimental-features nix-command eval --json --impure --expr '(import ./nix/sources.nix).my-git-repo.rev' | jq -r)
      if [ ! "$gitrev" = "$nivrev" ]; then
        echo "Mismatched revs: $gitrev != $nivrev"
        exit 42
      fi

      # here we cheat a bit and use "outPath", which actually is the result of
      # builtins.fetchGit.
      nivnixrev=$(nix --extra-experimental-features nix-command eval --json --impure --expr '(import ./nix/sources.nix).my-git-repo.outPath.rev' | jq -r)
      if [ ! "$gitrev" = "$nivnixrev" ]; then
        echo "Mismatched revs: $gitrev != $nivnixrev"
        exit 42
      fi
      nivnixrevcount=$(nix --extra-experimental-features nix-command eval --impure --json --expr '(import ./nix/sources.nix).my-git-repo.outPath.revCount')
      if [ ! "1" -eq "$nivnixrevcount" ]; then
        echo "Mismatched revCount: 1 != $nivnixrevcount"
        exit 42
      fi

      niv update my-git-repo -b branch
      nivrev2=$(nix --extra-experimental-features nix-command eval --impure --json --expr '(import ./nix/sources.nix).my-git-repo.rev' | jq -r)
      if [ ! "$gitrev2" = "$nivrev2" ]; then
        echo "Mismatched revs: $gitrev2 != $nivrev2"
        exit 42
      fi

      popd > /dev/null

      touch $out
    ''
  )
