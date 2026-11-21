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
in
{
  services.kiwix-serve = {
    enable = true;
    address = "127.0.0.1";
    inherit port;
    library = lib.mapAttrs (_: pkgs.fetchurl) (lib.importJSON ./zims.json) // {
      # the rest are not on download.kiwix.org, so kiwix-update cannot bump them
      # built and uploaded by `rfc-zim`, which rewrites the pin
      rfc_en_all = pkgs.fetchurl (lib.importJSON ./rfc.json);
      # keep it at pkgs.typst's version
      typst_en_docs = pkgs.fetchurl {
        url = "https://typst-community.github.io/extra-docs/zim/typst.app_docs_v0.14.2_b1dfeb16-2025-12-19.zim";
        hash = "sha256-VwLkOWA0rtTE+Lp9lRJJSds00DyGxOaFwOlcK2K02S0=";
      };
    };
  };

  services.nginx = {
    enable = true;
    # *.localhost resolves to loopback without /etc/hosts (systemd-resolved,
    # myhostname, browsers); *.local would be swallowed by mdns4_minimal
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
}
