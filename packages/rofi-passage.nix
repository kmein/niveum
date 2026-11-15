{
  writers,
  lib,
  rofi,
  passage,
  wtype,
  findutils,
  coreutils,
  gnused,
  gnugrep,
}:
writers.writeDashBin "rofi-passage" ''
  set -efu
  PATH=$PATH:${
    lib.makeBinPath [
      rofi
      passage
      wtype
      findutils
      coreutils
      gnused
      gnugrep
    ]
  }

  cd "''${PASSAGE_DIR:-$HOME/.passage/store}"
  name=$(find . -name '*.age' -printf '%P\n' | sed 's/\.age$//' | sort | rofi -dmenu -i -p passage) || exit 0
  [ -n "$name" ] || exit 0

  entry=$(passage show "$name")
  password=$(printf '%s\n' "$entry" | head -n 1)
  login=$(printf '%s\n' "$entry" | sed -n 's/^login: //p' | head -n 1)

  type() {
    printf '%s' "$1" | wtype -d 12 -
  }

  action=$({
    if [ -n "$login" ]; then echo autotype; fi
    echo password
    if [ -n "$login" ]; then echo login; fi
    if printf '%s\n' "$entry" | grep -q '^otpauth://totp/'; then echo otp; fi
  } | rofi -dmenu -i -p "$name") || exit 0

  case $action in
    autotype)
      type "$login"
      wtype -k Tab
      type "$password"
      ;;
    password) type "$password" ;;
    login) type "$login" ;;
    otp) type "$(passage otp "$name")" ;;
  esac
''
