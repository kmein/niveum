{ pkgs, lib, ... }:
let
  inherit (import <niveum/lib>) localAddresses;
in
{
  networking.firewall.allowedTCPPorts = [ 80 ];

  services.nginx = {
    enable = true;

    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;

   # Only allow PFS-enabled ciphers with AES256
   sslCiphers = "AES256+EECDH:AES256+EDH:!aNULL";
  };

  services.nextcloud = {
    enable = true;
    package = pkgs.nextcloud19;

    autoUpdateApps = {
      enable = true;
      startAt = "05:00:00";
    };

    hostName = localAddresses.toum;

    # https = true;
    config = {
      # overwriteProtocol = "https";
      dbtype = "pgsql";
      dbuser = "nextcloud";
      dbhost = "/run/postgresql"; # nextcloud will add /.s.PGSQL.5432 by itself
      dbname = "nextcloud";
      dbpass = lib.strings.fileContents <system-secrets/nextcloud/database>;
      adminpass = lib.strings.fileContents <system-secrets/nextcloud/admin>;
      adminuser = "admin";
      extraTrustedDomains = [ "toum.r" ];
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

  # Ensure that postgres is running before running the setup
  systemd.services."nextcloud-setup" = {
    requires = ["postgresql.service"];
    after = ["postgresql.service"];
  };
}
