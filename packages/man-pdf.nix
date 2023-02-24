{
  writers,
  ghostscript,
  man-db,
  gnused,
}:
writers.writeDashBin "man-pdf" ''
  set -efu
  ${man-db}/bin/man -t "$@" | ${ghostscript}/bin/ps2pdf - "$(echo "$*" | ${gnused}/bin/sed 's/\s\+/_/g').pdf"
''
