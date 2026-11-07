# Nix Projects

In a project whose dependencies come from a nixpkgs Haskell package set
(`shellFor`, `callCabal2nix`, …), `cabal` never fetches or builds them: GHC's
package database already holds them. `hypha` can still browse them, once you
tell it where their source lives.

## Why a nixpkgs project's dependencies have no source

`cabal` records every package of the build plan in
`dist-newstyle/cache/plan.json`. For a dependency it downloaded, that entry
carries a `pkg-src` saying where the source came from. For a dependency GHC
already had, the entry is a `pre-existing` unit with no `pkg-src` at all:
`cabal` never needed the source, so it never wrote it down.

`hypha` then has nothing to read, and falls back to downloading the tarball
Hackage publishes for that name and version. That is the wrong source for a
package nixpkgs patched or pinned to a git revision, and there is nothing to
download for a package Hackage never had.

The *plan overlay* closes the gap. Whoever built the package set knows where
each package's source is, and writes it into a file that `hypha` merges into
the plan.

## The overlay file

The overlay is a JSON file in `plan.json`'s own vocabulary: an `install-plan`
array with one entry per unit, giving the unit's `id` and a local `pkg-src`.

```json
{
  "install-plan": [
    {
      "id": "aeson-2.2.3.0-3XJYKsWd2IeEGqVp5VfmvT",
      "pkg-src": { "type": "local", "path": "/nix/store/…-aeson-2.2.3.0" }
    }
  ]
}
```

Only `id` and `pkg-src` are read; any other key of a `plan.json` unit is
accepted and ignored, so a fragment of a real `plan.json` is a valid overlay.
Each `id` may appear once.

### Finding the overlay

`hypha` looks for the overlay in this order, and uses the first that applies:

1. `--plan-overlay FILE` (see [Global Flags](flags.md));
2. the `HYPHA_PLAN_OVERLAY` environment variable. An empty variable counts as
   unset;
3. `hypha-plan-overlay.json` in the project root: the root `hypha` discovers,
   or the one `--project-dir` (and the MCP tools' `projectDir`) names.

A relative path given by the flag or the variable is relative to the working
directory. If neither names a file, the default is tried only if it exists;
nothing is reported when it does not.

### What the merge does

The overlay is merged into the plan by unit id whenever `hypha` loads the
build plan, which is where every source, module and component comes from. The
one reader of `plan.json` that does not apply it is the version check behind
`hypha lookup`: it reads only versions and unit ids, and an overlay changes
neither.

- A unit sourced this way **stays a dependency**: it is not a local package,
  `hypha package` reports its origin as `distribution` with the path, and the
  search index fingerprints its source by that store path.
- An entry for a unit `cabal` builds itself (your own packages) is **not
  applied**: `cabal` already knows where that source is.
- An entry for a unit the plan does not list is **counted, not an error**. A
  nix closure covers more than any one plan.
- A file named by the flag or the variable that **does not exist** is a
  warning on stderr, and the plan loads as if no overlay applied (the default
  path is not tried in its place).
- A file that cannot be read, is not valid JSON of the shape above, or names
  an id twice **fails the plan load**, naming the file.

A source asked for at a version other than the plan's, with `PKG-VER` or
`--package-override`, is never answered from the plan's source directory, the
overlay's included: it is looked up as for any package without one (the cabal
store, `hypha`'s source cache, Hackage).

## Building an overlay with Nix

`nix/plan-overlay.nix` in the `hypha` repository is a plain function taking
`{ pkgs, packages }`. It closes `packages` over their Haskell library
dependencies and writes one entry per unit id found in each package's
`package.conf.d`, pointing at that package's unpacked, patched source
(`pkgs.srcOnly`). A package with several libraries gets an entry for each.
Its output is the overlay file itself.

Pass the project's **dependencies**, not the project. The helper closes its
list over library dependencies only, and builds every derivation it is given,
so passing the project would build it, tests included, to produce entries
that are never applied, and would still miss the dependencies of its test
suite. A project's dependencies of every kind are in its `getCabalDeps`.

Build the overlay from **the package set your shell is built with**, overrides
included. Keep that set in one file that the shell and the overlay both import,
so there is never a second copy of your overrides to fall out of step:

```nix
# haskell-packages.nix
{ pkgs }:
pkgs.haskellPackages.override {
  overrides = final: prev: {
    myproj = final.callCabal2nix "myproj" ./. { };
    # …your patched and re-pinned dependencies, and any other overrides
  };
}
```

```nix
# shell.nix
let
  pkgs = import <nixpkgs> { };
  haskellPackages = import ./haskell-packages.nix { inherit pkgs; };
in
haskellPackages.shellFor { packages = p: [ p.myproj ]; }
```

```nix
# overlay.nix
let
  pkgs = import <nixpkgs> { };
  hypha = builtins.fetchTarball
    "https://github.com/well-typed/hypha/archive/<revision>.tar.gz";

  haskellPackages = import ./haskell-packages.nix { inherit pkgs; };
  myproj = haskellPackages.myproj;

  # getCabalDeps lists only the kinds the derivation was built for: tests
  # with doCheck, benchmarks with doBenchmark. `or [ ]` skips the others.
  # setupHaskellDepends are the dependencies of a `custom-setup` stanza,
  # which shellFor puts into the shell as well.
  kinds = [
    "libraryHaskellDepends"
    "executableHaskellDepends"
    "testHaskellDepends"
    "benchmarkHaskellDepends"
    "setupHaskellDepends"
  ];
in
import "${hypha}/nix/plan-overlay.nix" {
  inherit pkgs;
  packages = builtins.concatMap (kind: myproj.getCabalDeps.${kind} or [ ]) kinds;
}
```

```console
$ nix-build overlay.nix -o hypha-plan-overlay.json
```

The result is the file `hypha` finds by default in the project root. A
dependency that nixpkgs does not have as a derivation, which is what GHC's
boot packages are (`base`, `containers`, `text`, …), gets no entry.

**An overlay built from any other package set is wrong without saying so.** A
dependency you patched or re-pinned at the same version keeps its unit id,
because Cabal derives the id only from the package id, the ids of its
dependencies and the flags, and nixpkgs passes no `--ipid`. The overlay built from
a set without your patch therefore matches your shell's ids and points at the
unpatched source, and [`hypha doctor`](#what-hypha-doctor-reports) passes: it
sees only that every id is covered.

`getCabalDeps` is not a public interface. nixpkgs says of it, in
`generic-builder.nix`, that it is "intended just for `shellFor`" and "should be
considered unstable", so a change in nixpkgs may break this idiom.

### Through the flake

`hypha`'s flake exports the same function as `lib.planOverlay`, so a flake
that has `hypha` as an input does not need to fetch the source itself:

```nix
{
  inputs.hypha.url = "github:well-typed/hypha";
  # …
  outputs = { self, nixpkgs, hypha, ... }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
      # One package set, overrides included, for the shell and the overlay.
      haskellPackages = pkgs.haskellPackages.override {
        overrides = final: prev: {
          myproj = final.callCabal2nix "myproj" ./. { };
          # …your patched and re-pinned dependencies, and any other overrides
        };
      };
      myproj = haskellPackages.myproj;
      kinds = [
        "libraryHaskellDepends"
        "executableHaskellDepends"
        "testHaskellDepends"
        "benchmarkHaskellDepends"
        "setupHaskellDepends"
      ];
    in {
      devShells.x86_64-linux.default =
        haskellPackages.shellFor { packages = p: [ p.myproj ]; };

      packages.x86_64-linux.plan-overlay = hypha.lib.planOverlay {
        inherit pkgs;
        packages = builtins.concatMap (kind: myproj.getCabalDeps.${kind} or [ ]) kinds;
      };
    };
}
```

```console
$ nix build .#plan-overlay -o hypha-plan-overlay.json
```

The helper takes only `runCommand`, `srcOnly`, `jq` and `lib` from `pkgs`; the
sources and unit ids come from the derivations you pass as `packages`, so they
are those of the package set you took them from, not of a nixpkgs `hypha` was
tested against.

### Keeping it current

An overlay describes one package set. Rebuild it whenever any package's
source changes, with the same `nix-build … -o hypha-plan-overlay.json`.
Building it from the same expression as your shell, as above, is what keeps its
sources right: an overlay built from a different set can point a patched
dependency at the wrong source and nothing will say so. It also lets one
command refresh both.

[`hypha doctor`](#what-hypha-doctor-reports) names the dependencies the
overlay left without a source, which catches a changed version or a changed
set of dependencies. It does **not** catch a new patch or a moved git pin at
the same version: those keep their unit id, so the old entry still applies,
and the source it points at is the old one.

The link `nix-build -o` makes is specific to your machine, and is what keeps
the sources from `nix-collect-garbage`. Add `hypha-plan-overlay.json` to
`.gitignore`; never commit it. Likewise, keep the file out of `.hypha/`, which
is a disposable cache you may delete at any time (see [Caching](caching.md)).

## What `hypha doctor` reports

[`hypha doctor`](subcommands.md) has a `plan_overlay` check. It says:

- where the overlay came from (`--plan-overlay`, `HYPHA_PLAN_OVERLAY` or the
  default path) and its path;
- how many units it sourced;
- how many entries were not applied, split into those naming a unit `cabal`
  configures itself and those naming no unit of the plan (normal);
- how many `pre-existing` units are left without a source, and which. GHC's
  boot packages are always among them; any other is a dependency the overlay
  does not cover.

It is a warning, not a pass, when a file named by the flag or the variable
does not exist, or when the overlay sourced no unit of the plan (it was built
for some other package set). A malformed overlay fails the check. With no
overlay at all, the check passes and names the places it looked.

## What the overlay does not cover

- GHC's boot packages: nixpkgs has no derivation for them, and their only
  source is GHC's whole source tarball.
- A package set other than the one the overlay was built from, and any
  change of a package's source that keeps its unit id: see
  [Keeping it current](#keeping-it-current).
