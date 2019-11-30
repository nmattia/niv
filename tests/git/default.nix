{ pkgs, niv }:

# TODO: this doesn' test anything meaningful yet because "niv git PACKAGE"
# doesn't parse yet
pkgs.runCommand "foo"
  { nativeBuildInputs = [ pkgs.git niv ]; }
  ''
    gitdir=$(mktemp -d)

    pushd $gitdir
    git init .
    echo hello > file
    git config user.email "niv@foo.bar"
    git config user.name "Niv Niverson"
    git add file
    git commit -m "Initial commit"
    gitrev=$(git rev-parse HEAD)
    popd

    nivdir=$(mktemp -d)
    pushd $nivdir
    mkdir -p nix
    echo "{}" > nix/sources.json
    niv init
    # niv add git -n my-git-repo file://$gitdir
    # nix eval --json '(import ./nix/sources.nix).my-git-repo.rev'
    popd

    touch $out
  ''

