{pkgs, ...}: let
  inherit (import ../../lib) kieran;
in {
  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    sslCiphers = "AES256+EECDH:AES256+EDH:!aNULL";
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = kieran.email;
  };

  services.matomo = {
    enable = true;
    hostname = "matomo.kmein.de";
    nginx = {
      serverName = "matomo.kmein.de";
    };
  };

  services.mysql = {
    enable = true;
    package = pkgs.mariadb;
    ensureDatabases = ["matomo"];
    ensureUsers = [
      {
        name = "matomo";
        ensurePermissions."matomo.*" = "ALL PRIVILEGES";
      }
    ];
  };

  services.mysqlBackup = {
    enable = true;
    databases = ["matomo"];
  };
}
