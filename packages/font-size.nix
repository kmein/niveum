{ writeDashBin, font }:
writeDashBin "font-size" ''
  set -efu

  # set_font NORMAL_FONT BOLD_FONT
  set_font() {
    printf '\033]710;%s\007' "$1"
    printf '\033]711;%s\007' "$2"
  }

  case ''${1-} in
    '''|0|--reset)
      set_font \
          "xft:${font.name}:size=${toString font.size}" \
          "xft:${font.name}:size=${toString font.size}:bold" \
      ;;
    [2-9]|[1-9][0-9]|[1-9][0-9][0-9])
      set_font \
          "xft:${font.name}:size=$1" \
          "xft:${font.name}:size=$1:bold" \
      ;;
    *)
      echo "$0: bad argument: $1" >&2
      exit 1
  esac
''
