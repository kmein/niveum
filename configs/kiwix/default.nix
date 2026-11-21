# Offline reference library: DevDocs, ArchWiki, Gentoo wiki and more as ZIM
# files, served locally by kiwix-serve at http://docs.localhost. Bump the
# download.kiwix.org pins in zims.json with `kiwix-update`.
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
    library = lib.mapAttrs (_: pkgs.fetchurl) (lib.importJSON ./zims.json) // {
      # not on download.kiwix.org, so kiwix-update cannot bump it; keep it at pkgs.typst's version
      typst_en_docs = pkgs.fetchurl {
        url = "https://typst-community.github.io/extra-docs/zim/typst.app_docs_v0.14.2_b1dfeb16-2025-12-19.zim";
        hash = "sha256-VwLkOWA0rtTE+Lp9lRJJSds00DyGxOaFwOlcK2K02S0=";
      };
    };
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
