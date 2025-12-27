{
  st,
  fzf,
  writers,
  dash,
  lib,
}:
writers.writeBashBin "fzfmenu" ''
  # fzfmenu - fzf as dmenu replacement
  # https://github.com/junegunn/fzf/wiki/Examples#fzf-as-dmenu-replacement
  set -efu

  PATH=$PATH:${
    lib.makeBinPath [
      st
      fzf
      dash
    ]
  }

  input=$(mktemp -p "$XDG_RUNTIME_DIR" -u --suffix .fzfmenu.input)
  output=$(mktemp -p "$XDG_RUNTIME_DIR" -u --suffix .fzfmenu.output)
  mkfifo "$input"
  mkfifo "$output"
  chmod 600 "$input" "$output"

  for i in "$@"; do
    case $i in
      -p)
        PROMPT="$2"
        shift
        shift
        break ;;
      -l)
        # no reason to filter number of lines
        shift
        shift
        break ;;
      -i)
        # we do this anyway
        shift
        break ;;
      *)
        echo "Unknown option $1" >&2
        shift ;;
    esac
  done

  # it's better to use st here (starts a lot faster than pretty much everything else)
  st -c fzfmenu -n fzfmenu -g 85x10 \
    -e dash \
    -c "cat $input | fzf --reverse --prompt="''${PROMPT+> }" --print-query $* | tee $output" & disown

  # handle ctrl+c outside child terminal window
  trap 'kill $! 2>/dev/null; rm -f $input $output' EXIT

  cat > "$input"
  cat "$output"
''
