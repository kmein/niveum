{ pkgs, ... }:
rec {
  writeTOML = object: pkgs.runCommand "generated.toml" {} ''
    echo '${builtins.toJSON object}' | ${pkgs.remarshal}/bin/json2toml > $out
  '';
  toTOML = object: builtins.readFile (writeTOML object);
}
