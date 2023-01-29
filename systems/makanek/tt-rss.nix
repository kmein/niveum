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
    adminCredentialsFile = pkgs.writeText "miniflux" ''
      ADMIN_USERNAME='kfm'
      ADMIN_PASSWORD='${lib.strings.fileContents <secrets/miniflux/password>}'
    '';
    config = {
      FETCH_YOUTUBE_WATCH_TIME = "1";
      POLLING_FREQUENCY = "20";
      PORT = toString port;
      BASE_URL = "https://feed.kmein.de";
      # POCKET_CONSUMER_KEY = ...
    };
  };

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
