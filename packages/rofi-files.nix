{
  writers,
  lib,
  rofi,
  fd,
  xdg-utils,
}:
writers.writeDashBin "rofi-files" ''
  set -efu
  cd "$HOME"
  # fd skips hidden and gitignored paths, which keeps caches and build output out
  path=$(${lib.getExe fd} --type file --type directory | ${lib.getExe rofi} -dmenu -i -p open) || exit 0
  exec ${lib.getExe' xdg-utils "xdg-open"} "$HOME/$path"
''
