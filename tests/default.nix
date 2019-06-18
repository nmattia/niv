# Test the niv binary
#
# A webserver is started to mock API requests to GitHub and to serve
# files. niv commands are then executed and the some elements of the
# resulting sources.json are verified with jq.
#
# Note that niv needs to be patched to use http://localhost:3333
# instead of https://github.com since it doesn't support another
# GitHub url (while the GitHub API permits that).
#
# To mock GitHub, JSON responses of the real GitHub API have been
# copied to files (in the data folder) and are used by the webserver
# to serve GitHub API responses.

{ pkgs, niv }:

let
  niv_HEAD = "a489b65a5c3a29983701069d1ce395b23d9bde64";
  niv_HEAD- = "abc51449406ba3279c466b4d356b4ae8522ceb58";
  nixpkgs-channels_HEAD = "571b40d3f50466d3e91c1e609d372de96d782793";
  nivForTest = niv.overrideDerivation(old: {
    # TODO: Remove this patch by adding an argument to the github
    # subcommand to support GitHub entreprise.
    prePatch = ''
      sed "/import Data.Text.Encoding (encodeUtf8)/d" -i src/Niv/GitHub.hs
      sed "/import System.Environment (lookupEnv)/d" -i src/Niv/GitHub.hs
      sed "s|token <- fmap (GH.OAuth . encodeUtf8 . T.pack) <$> lookupEnv \"GITHUB_TOKEN\"|let token = Just (GH.EnterpriseOAuth \"http://localhost:3333\" \"\")|" -i src/Niv/GitHub.hs
      sed "s|https://github.com|http://localhost:3333|" -i src/Niv/GitHub.hs
    '';
  });
in pkgs.runCommand "test"
    { buildInputs =
        [ pkgs.haskellPackages.wai-app-static
          nivForTest
          pkgs.nix
          pkgs.jq
          pkgs.netcat-gnu
        ];
    }
  ''
    set -euo pipefail

    echo *** Starting the webserver...
    mkdir -p mock

    warp -d mock -p 3333 &

    while ! nc -z 127.0.0.1 3333; do
      echo waiting for mock server
      sleep 1
    done

    # We can't access /nix or /var from the sandbox
    export NIX_STATE_DIR=$PWD/nix/var/nix
    export NIX_STORE_DIR=$PWD/nix/store


    echo -e "\n*** niv init"

    ## mock API behavior:
    ##  - niv points to HEAD
    ##  - nixpkgs-channels points to HEAD

    mkdir -p mock/repos/nmattia/niv/
    cp  ${./data/repos/nmattia/niv/repository.json} mock/repos/nmattia/niv/index.html
    # XXX: cat so we don't inherit the read-only permissions
    cat ${./data/repos/nmattia/niv/commits.json} > mock/repos/nmattia/niv/commits
    mkdir -p mock/nmattia/niv/archive
    cp ${./data/archives + "/${niv_HEAD}.tar.gz"} \
      mock/nmattia/niv/archive/${niv_HEAD}.tar.gz

    mkdir -p mock/repos/NixOS/nixpkgs-channels
    cp  ${./data/repos/NixOS/nixpkgs-channels/repository.json} mock/repos/NixOS/nixpkgs-channels/index.html
    cat ${./data/repos/NixOS/nixpkgs-channels/commits.json} > mock/repos/NixOS/nixpkgs-channels/commits
    mkdir -p mock/NixOS/nixpkgs-channels/archive
    cp ${./data/archives + "/${nixpkgs-channels_HEAD}.tar.gz"} \
      mock/NixOS/nixpkgs-channels/archive/${nixpkgs-channels_HEAD}.tar.gz

    niv init
    diff -h ${./expected/niv-init.json} nix/sources.json || \
      (echo "Mismatched sources.json"; \
      echo "Reference: tests/expected/niv-init.json"; \
      exit 1)

    echo "*** ok."


    echo -e "\n*** niv drop niv"
    niv drop niv
    echo -n "niv package has been removed: "
    cat nix/sources.json  | jq -e '. | has("niv") | not'
    echo -e "*** ok."

    ## mock API behavior:
    ##  - niv points to HEAD~
    ##  - nixpkgs-channels points to HEAD

    echo -e "\n*** niv add nmattia/niv"
    # We use the HEAD~1 commit to update it in the next step
    # (e.g. we drop the first element of the commit array)
    cat ${./data/repos/nmattia/niv/commits.json} | jq 'del(.[0])' > mock/repos/nmattia/niv/commits
    cp ${./data/archives + "/${niv_HEAD-}.tar.gz"} \
      mock/nmattia/niv/archive/${niv_HEAD-}.tar.gz
    niv add nmattia/niv
    echo -n "niv.rev == ${niv_HEAD-} (HEAD~): "
    cat nix/sources.json | jq -e '.niv | .rev == "${niv_HEAD-}"'
    echo -e "*** ok."

    ## mock API behavior:
    ##  - niv points to HEAD
    ##  - nixpkgs-channels points to HEAD

    echo -e "\n*** niv update niv"
    cat ${./data/repos/nmattia/niv/commits.json} | jq '.[0] | [.]' > mock/repos/nmattia/niv/commits
    niv update niv
    echo -n "niv.rev == ${niv_HEAD} (HEAD): "
    cat nix/sources.json | jq -e '.niv | .rev == "${niv_HEAD}"'
    echo -e "*** ok."

    echo -e "\n*** niv add foo -v 1 -t 'localhost:3333/foo-v<version>'"
    echo foo > mock/foo-v1
    niv add foo -v 1 -t 'localhost:3333/foo-v<version>'
    echo -n "foo.url == localhost:3333/foo-v1: "
    cat nix/sources.json | jq -e '.foo | .url == "localhost:3333/foo-v1"'
    echo "*** ok."
    cp nix/sources.json $out
  ''
