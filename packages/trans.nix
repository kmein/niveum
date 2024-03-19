{
  fetchurl,
  writers,
  gawk,
}: let
  script = fetchurl {
    url = "https://raw.githubusercontent.com/soimort/translate-shell/gh-pages/trans.awk";
    hash = "sha256-KT5iRRGtHpBTrPfs0L2e4JW6JrXVTVvgCXeFKFcr1P4=";
  };
in
  writers.writeDashBin "trans" ''
    exec ${gawk}/bin/gawk -f ${script} -- "$@"
  ''
