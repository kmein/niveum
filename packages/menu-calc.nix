{
  writers,
  gnused,
  pari,
  dmenu,
  xclip,
}:
writers.writeDashBin "=" ''
  # https://github.com/onespaceman/menu-calc

  answer=$(echo "$@" | ${pari}/bin/gp -q | ${gnused}/bin/sed '/\./ s/\.\{0,1\}0\{1,\}$//')

  action=$(printf "copy\nclear" | ${dmenu}/bin/dmenu -p "= $answer")

  case $action in
    "clear") $0 ;;
    "copy") printf %s "$answer" | ${xclip}/bin/xclip -selection clipboard;;
    "") ;;
    *) $0 "$answer $action" ;;
  esac
''
