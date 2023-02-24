{
  fetchurl,
  writers,
  gawk,
}: let
  script = fetchurl {
    url = "https://raw.githubusercontent.com/soimort/translate-shell/gh-pages/trans.awk";
    sha256 = "178r8d27bry1mzd1g8x2svp4w469hwv7nnxnmnsinx974skjx0jb";
  };
in
  writers.writeDashBin "trans" ''
    exec ${gawk}/bin/gawk -f ${script} -- "$@"
  ''
