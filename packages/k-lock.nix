{
  writers,
  lib,
  xlockmore,
}:
let
  xlockModes = lib.concatStringsSep "\\n" [
    # "braid"
    "galaxy"
    # "lightning"
    # "matrix"
    "pyro2"
    "space"
  ];
in
writers.writeDashBin "k-lock" ''
  MODE=$(printf "${xlockModes}" | shuf -n 1)

  ${xlockmore}/bin/xlock \
    -saturation 0.4 \
    -erasemode no_fade \
    +description \
    -showdate \
    -username " " \
    -password " " \
    -info " " \
    -validate "..." \
    -invalid "Computer says no." \
    -mode "$MODE"
''
