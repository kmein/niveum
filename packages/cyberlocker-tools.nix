{
  lib,
  symlinkJoin,
  writers,
  curl,
}:
symlinkJoin {
  name = "cyberlocker-tools";
  paths = [
    (writers.writeDashBin "cput" ''
      set -efu
      path=''${1:-$(hostname)}
      path=$(echo "/$path" | sed -E 's:/+:/:')
      url=http://c.r$path

      ${lib.getExe curl} -fSs --data-binary @- "$url"
      echo "$url"
    '')
    (writers.writeDashBin "cdel" ''
      set -efu
      path=$1
      path=$(echo "/$path" | sed -E 's:/+:/:')
      url=http://c.r$path

      ${lib.getExe curl} -f -X DELETE "$url"
    '')
  ];
}
