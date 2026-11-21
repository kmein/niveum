{
  writers,
  lib,
  rofi,
  fd,
  xdg-utils,
  dragon-drop,
}:
writers.writeDashBin "rofi-files" ''
  set -efu
  cd "$HOME"
  # rofi-files: open the chosen file; rofi-files drag: drag the chosen files out with dragon-drop
  case "''${1:-open}" in
    open)
      # fd skips hidden and gitignored paths, which keeps caches and build output out
      path=$(${lib.getExe fd} --type file --type directory | ${lib.getExe rofi} -dmenu -i -p open) || exit 0
      exec ${lib.getExe' xdg-utils "xdg-open"} "$HOME/$path"
      ;;
    drag)
      paths=$(${lib.getExe fd} --type file --type directory | ${lib.getExe rofi} -dmenu -i -multi-select -p drag) || exit 0
      # absolute, since dragon's --stdin skips lines starting with -
      printf '%s\n' "$paths" | while IFS= read -r p; do printf '%s\n' "$HOME/$p"; done |
        exec ${lib.getExe dragon-drop} --and-exit --all --stdin
      ;;
    *)
      echo "usage: rofi-files [drag]" >&2
      exit 1
      ;;
  esac
''
