{
  pkgs,
  lib,
  config,
  ...
}: let
  domain = "feed.kmein.de";
  port = 8181;
in {
  services.miniflux = {
    enable = true;
    adminCredentialsFile = config.age.secrets.miniflux-credentials.path;
    config = {
      FETCH_YOUTUBE_WATCH_TIME = "1";
      POLLING_FREQUENCY = "20";
      PORT = toString port;
      BASE_URL = "https://feed.kmein.de";
      # POCKET_CONSUMER_KEY = ...
    };
  };

  age.secrets.miniflux-credentials.file = ../../secrets/miniflux-credentials.age;

  services.postgresqlBackup = {
    enable = true;
    databases = ["miniflux"];
  };

  services.nginx.virtualHosts.${domain} = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString port}";
    };
  };
}
