{
  writers,
  fetchurl,
  xan,
}: let
  database = fetchurl {
    url = "http://c.krebsco.de/greek.csv";
    hash = "sha256-SYL10kerNI0HzExG6JXh765+CBBCHLO95B6OKErQ/sU=";
  };
in
  writers.writeDashBin "heuretes" ''
    ${xan}/bin/xan search -s simple "^$*$" ${database} | ${xan}/bin/xan table
  ''
