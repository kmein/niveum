#!/bin/sh
set -efu
PROMPT="fzfmenu: "

for i in "$@"; do
  case $i in
    -p)
      PROMPT="$2"
      shift
      shift
      break ;;
    -l)
      # no reason to filter number of lines
      LINES="$2"
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

INPUT=$(cat)
OUTPUT="$(mktemp)"
alacritty \
  --title fzfmenu \
  --dimensions 85 15 \
  -e sh -c \
    "echo \"$INPUT\" | fzf \
      --history=/dev/null \
      --no-sort \
      --prompt=\"$PROMPT\" \
      > \"$OUTPUT\"" 2>/dev/null
cat "$OUTPUT"
rm "$OUTPUT"
