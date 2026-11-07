# Write a hypha plan overlay for Haskell packages that nixpkgs built.
#
# cabal never fetches a unit that GHC's package database already holds, so a
# plan built against nixpkgs' Haskell packages lists those units as
# `pre-existing`, with no `pkg-src`, and hypha cannot show their source. The
# overlay supplies it: one entry per unit id, pointing at the package's
# unpacked and patched source in the nix store.
#
#     import "${src}/nix/plan-overlay.nix" {
#       pkgs = import <nixpkgs> {};          # the caller's own package set
#       packages = [ pkgs.haskellPackages.aeson ];
#     }
#
# The result is a derivation whose output is the overlay JSON file itself:
#
#     { "install-plan": [ { "id": <unit id>,
#                           "pkg-src": { "type": "local", "path": <dir> } } ] }
#
# so `nix-build -E '…' -o hypha-plan-overlay.json` is the whole setup; hypha
# finds that file in the project root.
#
# `packages` is closed over the Haskell libraries they link against. GHC's
# boot packages stay unsourced: nixpkgs has no derivation for them (they are
# `null` in the package set), and their only source is GHC's whole tarball.
#
# Unit ids are read from the built packages' `package.conf.d`, at build time,
# so evaluating this performs no import-from-derivation.
{ pkgs, packages }:
let
  lib = pkgs.lib;

  # A Haskell package derivation, as opposed to `null` (a boot package) or
  # anything else that found its way into a dependency list.
  isHaskellPackage = d: lib.isDerivation d && d ? getCabalDeps;

  libraryDeps = d:
    lib.filter isHaskellPackage (d.getCabalDeps.libraryHaskellDepends or [ ]);

  # `packages` and everything their libraries depend on, each once, in a
  # stable order: genericClosure sorts by key.
  closure = builtins.genericClosure {
    startSet = map (d: { key = d.outPath; inherit d; })
      (lib.filter isHaskellPackage packages);
    operator = { d, ... }:
      map (dep: { key = dep.outPath; d = dep; }) (libraryDeps d);
  };

  # One line per package: its output and its source.
  pairs = lib.concatMapStrings (x: "${x.d} ${pkgs.srcOnly x.d}\n") closure;
in
pkgs.runCommand "hypha-plan-overlay.json"
  {
    nativeBuildInputs = [ pkgs.jq ];
    inherit pairs;
    passAsFile = [ "pairs" ];
  }
  ''
    entries=$(mktemp)
    while read -r drv src; do
      # An executable-only package has no lib/, and so no unit to source.
      [ -d "$drv/lib" ] || continue
      # Both layouts nixpkgs uses end in package.conf.d; a package can hold
      # several confs (sublibraries), each its own unit.
      find "$drv/lib" -path '*/package.conf.d/*.conf' | sort | while read -r conf; do
        # The value follows `id:` on the same line, or on the next when long.
        id=$(awk '
          /^id:/ { sub(/^id:[ \t]*/, ""); if ($0 != "") { print; exit } ; want = 1; next }
          want   { sub(/^[ \t]+/, ""); print; exit }
        ' "$conf")
        if [ -z "$id" ]; then
          echo "no id: line in $conf" >&2
          exit 1
        fi
        printf '%s\t%s\n' "$id" "$src"
      done >> "$entries"
    done < "$pairsPath"

    jq -R -s '
      { "install-plan":
          ( split("\n") | map(select(. != "") | split("\t"))
          | map({ id: .[0], "pkg-src": { type: "local", path: .[1] } })
          | unique_by(.id) ) }
    ' "$entries" > "$out"
  ''
