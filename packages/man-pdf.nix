{ writeShellScriptBin, texlive }:
writeShellScriptBin "man-pdf" ''
  for program in "$@"; do
    man -t "$program" | ${texlive.combined.scheme-basic}/bin/ps2pdf
''
