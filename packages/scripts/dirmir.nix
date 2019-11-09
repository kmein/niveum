{ writers, coreutils, findutils }:
let name = "dirmir";
in writers.writeDashBin name ''
  export PATH=$PATH:${coreutils}/bin:${findutils}/bin

  SOURCE="$1"
  TARGET="$2"

  if [ ! -d "$SOURCE" ] || [ $# -ne 2 ]; then
    echo >/dev/stderr "Usage: ${name} SOURCE TARGET"
    exit 1
  fi

  if [ -e "$TARGET" ]; then
    echo >/dev/stderr "$TARGET" already exists. Please use a different name.
    exit 1
  fi

  for entry in $(find "$SOURCE"); do
    if [ -d "$entry" ]; then
      mkdir -p "$TARGET/$entry"
    else
      # create a file with the same permissions as $entry
      install -m "$(stat -c %a "$entry")" /dev/null "$TARGET/$entry"
    fi
  done
''
