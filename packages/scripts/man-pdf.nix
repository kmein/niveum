{ writeShellScriptBin, ghostscript, man }:
let
  name = "man-pdf";
in writeShellScriptBin name ''
  if [ $# -eq 1 ]; then
    man_entry="$1"
  elif [ $# -eq 2 ]; then
    man_page="$1"
    man_entry="$2"
  else
    echo >/dev/stderr "Usage: ${name} [MAN-PAGE] ENTRY"
    exit 1
  fi

  echo "${man}/bin/man "''${man_page:-}" "$man_entry" | ${ghostscript}/bin/ps2pdf - "$man_entry.pdf""
  ${man}/bin/man "''${man_page:-}" "$man_entry" | ${ghostscript}/bin/ps2pdf - "$man_entry.pdf"
''
