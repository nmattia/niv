# HACKING

## Building

To build the project, we recommend using Cabal.

## Release Playbook

First, checkout the latest main:

```bash
git switch main && git pull
```

Make sure there are no changes. Then bump the version in the following files:

* `package.yaml`
* `CHANGELOG`
* `default.nix`

Then, make sure the auto-generated README.md is up to date:

```bash
./script/gen
```

Then, make sure the tests pass:

```bash
./script/test
```

Then, commit the release changes:

```bash
git commit -am "Release 3.14.15"
```

Then, generate the cabal sdist:

```bash
cp $(nix-build -A niv-sdist)/niv-<version>.tar.gz .
```

Then, upload the sdist:

```bash
cabal upload ./niv-<version>.tar.gz # use --publish when you're sure
```

Then, create a new tag for the version and push it:

```bash
git tag v3.14.15
git push
git push --tags
```
