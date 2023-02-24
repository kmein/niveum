{
  writers,
  ripgrep,
  fzf,
  gawk,
}:
writers.writeDashBin "vg" ''
  file="$(${ripgrep}/bin/rg "$@" | ${fzf}/bin/fzf -0 -1 | ${gawk}/bin/awk -F: '{print $1}')"

  if [ -n "$file" ]; then
    ''${EDITOR:-vim} "$file"
  fi
''
