# ref https://tex.stackexchange.com/a/502542
{
  writers,
  imagemagick,
}:
writers.writeDashBin "scanned" ''
  [ $# -eq 1 -a -f "$1" -a -r "$1" ] || exit 1

  ${imagemagick}/bin/convert \
    -density 150 \
    "$1" \
    -rotate 0.5 \
    -attenuate 0.25 \
    +noise Multiplicative \
    -colorspace Gray \
    "scanned-$1"
''
