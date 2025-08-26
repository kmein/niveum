{
  writers,
  mktemp,
  qrencode,
  wl-clipboard,
  nsxiv,
}:
writers.writeDashBin "qrpaste" ''
  file="$(${mktemp}/bin/mktemp --tmpdir)"
  trap clean EXIT
  clean() {
    rm "$file"
  }
  ${qrencode}/bin/qrencode "$(${wl-clipboard}/bin/wl-paste)" -o "$file"
  ${nsxiv}/bin/nsxiv "$file"
''
