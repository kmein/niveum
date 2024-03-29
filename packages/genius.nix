{
  writers,
  curl,
  coreutils,
  gnused,
  pandoc,
}:
writers.writeDashBin "genius" ''
  ${coreutils}/bin/test "$#" -eq 2 || (
    echo "usage: $0 <artist> <song>"
    exit 1
  )

  normalize() {
    ${coreutils}/bin/tr -d -c '0-9A-Za-z ' | ${coreutils}/bin/tr ' ' - | ${coreutils}/bin/tr '[:upper:]' '[:lower:]'
  }

  ARTIST=$(echo "$1" | normalize | ${gnused}/bin/sed 's/./\U&/')
  TITLE=$(echo "$2" | normalize)
  GENIUS_URL="https://genius.com/$ARTIST-$TITLE-lyrics"

  ${curl}/bin/curl -s "$GENIUS_URL" \
    | ${gnused}/bin/sed -ne '/class="lyrics"/,/<\/p>/p' \
    | ${pandoc}/bin/pandoc -f html -s -t plain \
    | ${gnused}/bin/sed 's/^_/\x1b[3m/g;s/_$/\x1b[0m/g;s/^\[/\n\x1b\[1m\[/g;s/\]$/\]\x1b[0m/g'

  printf "\n%s\n" "$GENIUS_URL" >/dev/stderr
''
