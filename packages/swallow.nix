{
  writers,
  xdo,
}:
# https://github.com/salman-abedin/devour/blob/master/devour.sh
writers.writeDashBin "swallow" ''
  id=$(${xdo}/bin/xdo id)
  ${xdo}/bin/xdo hide
  $("$@") > /dev/null 2>&1
  ${xdo}/bin/xdo show "$id"
''
