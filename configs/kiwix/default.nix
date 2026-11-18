# Offline reference library: DevDocs, ArchWiki and Gentoo wiki as ZIM files,
# served locally by kiwix-serve at http://docs.localhost. Bump the pins with
# `kiwix-update`.
{
  pkgs,
  lib,
  ...
}:
let
  port = 8095;
  # *.localhost resolves to loopback without /etc/hosts (systemd-resolved,
  # myhostname, browsers); *.local would be swallowed by mdns4_minimal
  url = "http://docs.localhost";
in
{
  services.kiwix-serve = {
    enable = true;
    address = "127.0.0.1";
    inherit port;
    library = lib.mapAttrs (_: pkgs.fetchurl) (lib.importJSON ./zims.json);
  };

  services.nginx = {
    enable = true;
    virtualHosts."docs.localhost" = {
      listen = [
        {
          addr = "127.0.0.1";
          port = 80;
        }
      ];
      locations."/".proxyPass = "http://127.0.0.1:${toString port}";
    };
  };

  environment.systemPackages = [
    # docs           open the library
    # docs <words>   full-text search across all ZIMs
    (pkgs.writers.writeDashBin "docs" ''
      if [ "$#" -eq 0 ]; then
        exec ${lib.getExe' pkgs.xdg-utils "xdg-open"} ${url}
      fi
      pattern=$(${lib.getExe pkgs.jq} -rn --arg q "$*" '$q | @uri')
      exec ${lib.getExe' pkgs.xdg-utils "xdg-open"} "${url}/search?pattern=$pattern"
    '')
  ];
}
