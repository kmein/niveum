#!/bin/sh
test "$#" -eq 2 || (
  echo "usage: $0 <artist> <song>"
  exit 1
)

normalize() {
  tr -d -c '0-9A-Za-z ' | tr ' ' - | tr '[:upper:]' '[:lower:]'
}

ARTIST=$(echo "$1" | normalize | sed 's/./\U&/')
TITLE=$(echo "$2" | normalize)
GENIUS_URL="https://genius.com/$ARTIST-$TITLE-lyrics"

curl -s "$GENIUS_URL" \
  | sed -ne '/class="lyrics"/,/<\/p>/p' \
  | pandoc -f html -s -t plain \
  | sed 's/^_/\x1b[3m/g;s/_$/\x1b[0m/g;s/^\[/\n\x1b\[1m\[/g;s/\]$/\]\x1b[0m/g'

printf "\n%s\n" "$GENIUS_URL" >/dev/stderr
