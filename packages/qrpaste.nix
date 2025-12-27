{
  writers,
  mktemp,
  qrencode,
  xclip,
  nsxiv,
}:
writers.writeDashBin "qrpaste" ''
  file="$(${mktemp}/bin/mktemp -p "$XDG_RUNTIME_DIR" qrpaste-XXXXXX.png)"
  trap clean EXIT
  clean() {
    rm "$file"
  }
  ${qrencode}/bin/qrencode "$(${xclip}/bin/xclip -selection clipboard -out)" -o "$file"
  ${nsxiv}/bin/nsxiv "$file"
''
