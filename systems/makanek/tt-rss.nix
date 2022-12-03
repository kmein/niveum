{
  pkgs,
  lib,
  config,
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
      enable = false;
      maxUsers = 3;
    };
  };

  services.postgresqlBackup = {
    enable = true;
    databases = [config.services.tt-rss.database.name];
  };

  services.nginx.virtualHosts.${domain} = {
    enableACME = true;
    forceSSL = true;
  };
}
