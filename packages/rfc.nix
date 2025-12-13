{
  writers,
  curl,
  gawk,
  gnused,
  less,
  fzf,
}:
writers.writeDashBin "rfc" ''
  set -efu
  selection=$(
    ${curl}/bin/curl -sSL https://www.rfc-editor.org/rfc-index.txt \
    | ${gawk}/bin/awk '/^$/{print;} /./{printf("%s ", $0);}' \
    | ${gnused}/bin/sed 's/\s\+/ /g' \
    | ${gnused}/bin/sed -n '/^[0-9]\+ /,$p' \
    | ${fzf}/bin/fzf \
    | ${gawk}/bin/awk '{print $1}'
  )

  ${curl}/bin/curl -sSL "https://www.rfc-editor.org/rfc/rfc$selection.txt" | ${less}/bin/less
''
