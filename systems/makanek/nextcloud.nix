{
  pkgs,
  config,
  lib,
  ...
}: let
  passwordFile = path: toString (pkgs.writeText "password" (lib.strings.fileContents path));
  inherit (import <niveum/lib>) localAddresses;
in {
  services.nextcloud = {
    enable = true;
    package = pkgs.nextcloud25;

    https = true;

    autoUpdateApps = {
      enable = true;
      startAt = "05:00:00";
    };

    hostName = "cloud.xn--kiern-0qa.de";

    phpOptions."opcache.interned_strings_buffer" = "32"; # buffer size in MB

    config = {
      overwriteProtocol = "https";

      dbtype = "pgsql";
      dbuser = "nextcloud";
      dbhost = "/run/postgresql"; # nextcloud will add /.s.PGSQL.5432 by itself
      dbname = "nextcloud";
      dbpassFile = passwordFile <system-secrets/nextcloud/database>;
      adminpassFile = passwordFile <system-secrets/nextcloud/admin>;
      adminuser = "admin";
      # extraTrustedDomains = [ "toum.r" ];
      defaultPhoneRegion = "DE";
    };

    logLevel = 2;

    extraOptions = let
      inherit (import <niveum/lib/email.nix> {inherit lib;}) cock;
      address = builtins.split "@" cock.user;
    in {
      defaultapp = "files";
      mail_smtpmode = "smtp";
      mail_sendmailmode = "smtp";
      mail_smtphost = cock.smtp;
      mail_smtpport = "587";
      mail_from_address = builtins.elemAt address 0;
      mail_domain = builtins.elemAt address 2;
      mail_smtpsecure = "tls";
      mail_smtpauthtype = "LOGIN";
      mail_smtpauth = 1;
      mail_smtpname = cock.user;
      mail_smtppassword = cock.password;
    };
  };

  niveum.passport.services = [
    {
      title = "Nextcloud";
      link = "https://${config.services.nextcloud.hostName}";
      description = "manages calendars, to-do lists, files, and recipes.";
    }
  ];

  services.postgresql = {
    enable = true;
    ensureDatabases = ["nextcloud"];
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
