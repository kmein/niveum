{
  writers,
  fetchurl,
  xsv,
}: let
  database = fetchurl {
    url = "http://c.krebsco.de/greek.csv";
    hash = "sha256-SYL10kerNI0HzExG6JXh765+CBBCHLO95B6OKErQ/sU=";
  };
in
  writers.writeDashBin "heuretes" ''
    ${xsv}/bin/xsv search -s simple "^$*$" ${database} | ${xsv}/bin/xsv table
  ''
