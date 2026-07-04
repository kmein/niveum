{ pkgs, lib, ... }:
let
  domain = "matomo.${pkgs.lib.niveum.domain}";
in
{
  services.matomo = {
    enable = true;
    hostname = domain;
    nginx = {
      serverName = domain;
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
