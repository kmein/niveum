{ writeShellScriptBin }:
writeShellScriptBin "n" ''nix-shell -p "$1" --run "''${2:-$1}"''
