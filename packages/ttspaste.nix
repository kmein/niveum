{
  lib,
  writers,
  wl-clipboard,
  speechd,
  gnugrep,
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

  # speech-dispatcher picks the voice by language only, so guess it from the script.
  # urdu is arabic script plus letters arabic lacks; latin text goes by its diacritics
  export LC_ALL=C.UTF-8
  has() { printf %s "$text" | ${lib.getExe gnugrep} -qP "$1"; }
  if has '[\x{0679}\x{0688}\x{0691}\x{06BA}\x{06BE}\x{06C1}\x{06D2}]'; then lang=ur
  elif has '\p{Arabic}'; then lang=ar
  elif has '\p{Hebrew}'; then lang=he
  elif has '\p{Devanagari}'; then lang=hi
  elif has '\p{Greek}'; then lang=el
  elif has '[ąćęłńśźżĄĆĘŁŃŚŹŻ]'; then lang=pl
  elif has '[äöüßÄÖÜ]'; then lang=de
  else lang=en
  fi

  exec ${spd-say} --wait --language "$lang" -- "$text"
''
