{
  lib,
  writers,
  wl-clipboard,
  speechd,
  util-linux,
}:
let
  spd-say = lib.getExe' speechd "spd-say";
  wl-paste = lib.getExe' wl-clipboard "wl-paste";
in
# reads the selection, else the clipboard, with speech-dispatcher's voices; a second call stops it
writers.writeDashBin "ttspaste" ''
  exec 9>"''${XDG_RUNTIME_DIR:-/tmp}/ttspaste.lock"
  if ! ${lib.getExe' util-linux "flock"} -n 9; then
    exec ${spd-say} --cancel
  fi

  text=$(${wl-paste} --primary --no-newline 2>/dev/null)
  [ -n "$text" ] || text=$(${wl-paste} --no-newline 2>/dev/null)
  [ -n "$text" ] || exit 0

  exec ${spd-say} --wait --language en -- "$text"
''
