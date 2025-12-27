{ pkgs, lib, ... }:
{
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
    defaults.email = pkgs.lib.niveum.kieran.email;
  };

  services.matomo = {
    enable = true;
    hostname = "matomo.kmein.de";
    nginx = {
      serverName = "matomo.kmein.de";
    };
    package = pkgs.matomo;
  };

  services.mysql = {
    enable = true;
    package = pkgs.mariadb;
    ensureDatabases = [ "matomo" ];
    ensureUsers = [
      {
        name = "matomo";
        ensurePermissions."matomo.*" = "ALL PRIVILEGES";
      }
    ];
  };

  services.mysqlBackup = {
    enable = true;
    databases = [ "matomo" ];
  };
}
