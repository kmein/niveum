{
  curl,
  gnused,
  writers,
}:
writers.writeDashBin "kpaste" ''
  ${curl}/bin/curl -sS http://p.r --data-binary @"''${1:--}" |
  ${gnused}/bin/sed '$ {p;s|http://p.r|https://p.krebsco.de|}'
''
