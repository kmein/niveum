{ lib }:
with lib;
let
  callPackage = set: f: overrides: f ((builtins.intersectAttrs (builtins.functionArgs f) set) // overrides);
  subdirsOf = path: lib.mapAttrs (name: _: path + "/${name}") (filterAttrs (_: eq "directory") (readDir path));
in mapAttrs
  (_: flip callPackage {})
  (filterAttrs
    (_: dir: pathExists (dir + "/default.nix"))
    (subdirsOf ./.))
