{
  writers,
  wget,
  system,
}:
writers.writeDashBin "nix-index-update" ''
  filename="index-${system}"
  mkdir -p ~/.cache/nix-index
  cd ~/.cache/nix-index
  # -N will only download a new version if there is an update.
  ${wget}/bin/wget -q -N https://github.com/Mic92/nix-index-database/releases/latest/download/$filename
  ln -f $filename files
''
