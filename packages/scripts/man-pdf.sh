#!/bin/sh
if [ "$#" -eq 1 ]; then
  man_entry="$1"
elif [ $# -eq 2 ]; then
  man_page="$1"
  man_entry="$2"
else
  echo >/dev/stderr "Usage: $0 [MAN-PAGE] ENTRY"
  exit 1
fi

man "${man_page:-}" "$man_entry" | ps2pdf - "$man_entry.pdf"
