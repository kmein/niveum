{
  writers,
  mktemp,
  qrencode,
  xclip,
  nsxiv,
}:
writers.writeDashBin "qrpaste" ''
  file="$(${mktemp}/bin/mktemp --tmpdir)"
  trap clean EXIT
  clean() {
    rm "$file"
  }
  ${qrencode}/bin/qrencode "$(${xclip}/bin/xclip -selection clipboard -out)" -o "$file"
  ${nsxiv}/bin/nsxiv "$file"
''
