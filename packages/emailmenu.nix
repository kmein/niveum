{
  writers,
  lib,
  coreutils,
  dmenu,
  gawk,
  libnotify,
  xclip,
  khard,
}:
writers.writeDashBin "emailmenu" ''
  history_file=$HOME/.cache/emailmenu
  PATH=${
    lib.makeBinPath [
      coreutils
      dmenu
      gawk
      libnotify
      xclip
    ]
  }
  chosen=$(${khard}/bin/khard email --parsable | awk '!seen[$0]++' | dmenu -i -p 📧 -1 -l 10 | tee --append "$history_file" | cut -f1)
  [ "$chosen" != "" ] || exit
  echo "$chosen" | tr -d '\n' | xclip -selection clipboard
  notify-send --app-name="$(basename "$0")" "'$chosen' copied to clipboard." &
''
