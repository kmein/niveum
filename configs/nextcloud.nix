{ pkgs, lib, ... }:
let
  inherit (import <niveum/lib>) localAddresses;
in
{
  services.nextcloud = {
    enable = true;
    package = pkgs.nextcloud21;

    https = true;

    autoUpdateApps = {
      enable = true;
      startAt = "05:00:00";
    };

    hostName = "cloud.xn--kiern-0qa.de";

    config = {
      overwriteProtocol = "https";

      dbtype = "pgsql";
      dbuser = "nextcloud";
      dbhost = "/run/postgresql"; # nextcloud will add /.s.PGSQL.5432 by itself
      dbname = "nextcloud";
      dbpass = lib.strings.fileContents <system-secrets/nextcloud/database>;
      adminpass = lib.strings.fileContents <system-secrets/nextcloud/admin>;
      adminuser = "admin";
      # extraTrustedDomains = [ "toum.r" ];
    };
  };

  services.postgresql = {
    enable = true;
    ensureDatabases = [ "nextcloud" ];
    ensureUsers = [
      {
        name = "nextcloud";
        ensurePermissions."DATABASE nextcloud" = "ALL PRIVILEGES";
      }
    ];
  };


  services.nginx.virtualHosts."cloud.xn--kiern-0qa.de" = {
    enableACME = true;
    forceSSL = true;
  };

  # Ensure that postgres is running before running the setup
  systemd.services."nextcloud-setup" = {
    requires = ["postgresql.service"];
    after = ["postgresql.service"];
  };
}
