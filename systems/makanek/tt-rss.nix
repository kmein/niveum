{
  pkgs,
  lib,
  config,
  ...
}:
let
  domain = "feed.${pkgs.lib.niveum.domain}";
  port = 8181;
in
{
  services.miniflux = {
    enable = true;
    adminCredentialsFile = config.age.secrets.miniflux-credentials.path;
    config = {
      FETCH_YOUTUBE_WATCH_TIME = "1";
      POLLING_FREQUENCY = "20";
      PORT = toString port;
      BASE_URL = "https://${domain}";
      # POCKET_CONSUMER_KEY = ...
    };
  };

  age.secrets.miniflux-credentials.file = ../../secrets/miniflux-credentials.age;

  services.postgresqlBackup = {
    enable = true;
    databases = [ "miniflux" ];
  };

  # miniflux creates its database here; the data directory is still version 14
  services.postgresql.package = pkgs.postgresql_14;

  services.prometheus.exporters.postgres = {
    enable = true;
    listenAddress = "127.0.0.1";
    runAsLocalSuperUser = true; # peer auth over /run/postgresql, no credentials
  };

  services.nginx.virtualHosts.${domain} = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString port}";
    };
  };
}
