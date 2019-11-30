{ pkgs, niv }:

# TODO: this doesn' test anything meaningful yet because "niv git PACKAGE"
# doesn't parse yet
pkgs.runCommand "foo"
  { nativeBuildInputs = [ pkgs.git niv pkgs.nix pkgs.jq ]; }
  (
  # First we create a dummy git repo with a single commit
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
    popd > /dev/null
  '' +

  # Then we `niv add` that repo and check some properties, like the revision
  # and revCount, to make sure it was imported properly, and that sources.nix
  # does what it's supposed to do.
  ''
    nivdir=$(mktemp -d)
    pushd $nivdir > /dev/null
    mkdir -p nix
    echo "{}" > nix/sources.json
    niv init
    niv add git -n my-git-repo --repo file://$gitdir
    nivrev=$(nix eval --json '(import ./nix/sources.nix).my-git-repo.rev' | jq -r)
    if [ ! "$gitrev" = "$nivrev" ]; then
      echo "Mismatched revs: $gitrev != $nivrev"
      exit 42
    fi

    # here we cheat a bit and use "outPath", which actually is the result of
    # builtins.fetchGit.
    nivnixrev=$(nix eval --json '(import ./nix/sources.nix).my-git-repo.outPath.rev' | jq -r)
    if [ ! "$gitrev" = "$nivnixrev" ]; then
      echo "Mismatched revs: $gitrev != $nivnixrev"
      exit 42
    fi
    nivnixrevcount=$(nix eval --json '(import ./nix/sources.nix).my-git-repo.outPath.revCount')
    if [ ! "1" -eq "$nivnixrevcount" ]; then
      echo "Mismatched revCount: 1 != $nivnixrevcount"
      exit 42
    fi
    popd > /dev/null

    touch $out
  ''
  )

