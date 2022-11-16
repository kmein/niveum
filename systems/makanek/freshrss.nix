{
  pkgs,
  lib,
  ...
}: let
  domain = "feed.kmein.de";
in {
  imports = [<nixos-unstable/nixos/modules/services/web-apps/freshrss.nix>];

  nixpkgs.config.packageOverrides = pkgs: {
    freshrss = (import <nixos-unstable> {}).freshrss;
  };

  # services.tt-rss = {
  #   enable = true;
  #   logDestination = "syslog";
  #   root = "/var/lib/tt-rss";
  #   selfUrlPath = "https://${domain}";
  #   virtualHost = domain;
  #   registration = {
  #     enable = true;
  #     maxUsers = 3;
  #   };
  # };

  services.freshrss = {
    enable = true;
    baseUrl = "https://${domain}";
    dataDir = "/var/lib/freshrss";
    database.type = "sqlite";
    defaultUser = "k";
    passwordFile = pkgs.writeText "freshrss-password" (lib.strings.fileContents <secrets/freshrss/password>);
    virtualHost = domain;
  };

  services.nginx.virtualHosts.${domain} = {
    enableACME = true;
    forceSSL = true;
  };
}
