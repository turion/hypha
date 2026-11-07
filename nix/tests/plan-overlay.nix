# Builds a plan overlay over a small closure and checks it. The closure holds
# `aeson`, whose dependencies include GHC boot packages (`null` in the package
# set), and `alex`, an executable-only package with no unit to source.
#
#     nix-build --no-out-link nix/tests/plan-overlay.nix
{ pkgs ? import <nixpkgs> { } }:
let
  hp = pkgs.haskellPackages;

  overlay = import ../plan-overlay.nix {
    inherit pkgs;
    packages = [ hp.aeson hp.alex ];
  };
in
pkgs.runCommand "plan-overlay-check"
  {
    nativeBuildInputs = [ pkgs.jq ];
    inherit overlay;
    aeson = hp.aeson;
    alex = hp.alex;
    ghc = hp.ghc;
  }
  ''
    fail() { echo "plan-overlay check failed: $*" >&2; exit 1; }

    # The overlay format hypha parses: install-plan entries with an id and a
    # local pkg-src path, each id once. Extra fields would be ignored.
    jq -e '
      (keys == ["install-plan"])
      and (.["install-plan"] | type == "array" and length > 0)
      and (.["install-plan"] | all(
            (.id | type == "string")
            and (.["pkg-src"].type == "local")
            and (.["pkg-src"].path | type == "string")))
      and ((.["install-plan"] | map(.id)) as $ids | ($ids | length) == ($ids | unique | length))
    ' "$overlay" >/dev/null || fail "not in the overlay format"

    # Deterministic: entries sorted by id.
    jq -e '(.["install-plan"] | map(.id)) as $ids | $ids == ($ids | sort)' \
      "$overlay" >/dev/null || fail "entries are not sorted by id"

    # aeson's own unit id (its conf's file name, which is its id) is present.
    found=0
    for conf in $(find "$aeson/lib" -path '*/package.conf.d/aeson-[0-9]*.conf'); do
      id=$(basename "$conf" .conf)
      jq -e --arg id "$id" '.["install-plan"] | any(.id == $id)' "$overlay" >/dev/null \
        || fail "no entry for aeson's unit id $id"
      found=1
    done
    [ "$found" = 1 ] || fail "found no aeson conf to compare with"

    # No boot package: GHC's own package database holds their confs.
    for conf in $(find "$ghc/lib" -path '*/package.conf.d/*.conf'); do
      id=$(basename "$conf" .conf)
      if jq -e --arg id "$id" '.["install-plan"] | any(.id == $id)' "$overlay" >/dev/null; then
        fail "entry for the boot package $id"
      fi
    done

    # The executable-only package contributes nothing.
    ! find "$alex/lib" -name '*.conf' 2>/dev/null | grep -q . \
      || fail "alex was expected to be executable-only"
    jq -e '.["install-plan"] | all(.id | test("^alex-") | not)' "$overlay" >/dev/null \
      || fail "entry for the executable-only package alex"

    # Every entry's path holds the .cabal file of the package its id names:
    # the id is <name>-<version>-<hash>, with the name possibly holding dashes.
    jq -r '.["install-plan"][]
           | [ (.id | capture("^(?<name>.+?)-[0-9]+(\\.[0-9]+)*-") .name),
               .["pkg-src"].path, .id ]
           | @tsv' "$overlay" | while IFS=$'\t' read -r name path id; do
      [ -f "$path/$name.cabal" ] || fail "$id: no $name.cabal in $path"
    done

    touch $out
  ''
