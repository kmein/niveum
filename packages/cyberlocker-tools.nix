{pkgs}:
pkgs.symlinkJoin {
  name = "cyberlocker-tools";
  paths = [
    (pkgs.writers.writeDashBin "cput" ''
      set -efu
      path=''${1:-$(hostname)}
      path=$(echo "/$path" | sed -E 's:/+:/:')
      url=http://c.r$path

      ${pkgs.curl}/bin/curl -fSs --data-binary @- "$url"
      echo "$url"
    '')
    (pkgs.writers.writeDashBin "cdel" ''
      set -efu
      path=$1
      path=$(echo "/$path" | sed -E 's:/+:/:')
      url=http://c.r$path

      ${pkgs.curl}/bin/curl -f -X DELETE "$url"
    '')
  ];
}
