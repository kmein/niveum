{
  pkgs,
  lib,
  ...
}: let
  domain = "feed.kmein.de";
in {
  services.tt-rss = {
    enable = true;
    logDestination = "syslog";
    root = "/var/lib/tt-rss";
    selfUrlPath = "https://${domain}";
    virtualHost = domain;
    registration = {
      enable = true;
      maxUsers = 3;
    };
  };

  services.nginx.virtualHosts.${domain} = {
    enableACME = true;
    forceSSL = true;
  };
}
