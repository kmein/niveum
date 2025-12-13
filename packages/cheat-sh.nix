# https://nitter.net/igor_chubin/status/1557793569104183298
{
  writers,
  curl,
}:
writers.writeDashBin "cht.sh" ''
  IFS=+
  ${curl}/bin/curl -sSL http://cht.sh/"$*"
''
