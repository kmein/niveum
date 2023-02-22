{
  pkgs,
  config,
  lib,
  ...
}: let
  inherit (import ../../lib) localAddresses;
in {
  age.secrets = {
    nextcloud-password-database = {
      file = ../../secrets/nextcloud-password-database.age;
      owner = "nextcloud";
      group = "nextcloud";
      mode = "440";
    };
    nextcloud-password-admin = {
      file = ../../secrets/nextcloud-password-admin.age;
      owner = "nextcloud";
      group = "nextcloud";
      mode = "440";
    };
  };

  services.nextcloud = {
    enable = true;
    package = pkgs.nextcloud25;

    https = true;
    enableBrokenCiphersForSSE = false;

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
      dbpassFile = config.age.secrets.nextcloud-password-database.path;
      adminpassFile = config.age.secrets.nextcloud-password-admin.path;
      adminuser = "admin";
      # extraTrustedDomains = [ "toum.r" ];
      defaultPhoneRegion = "DE";
    };

    logLevel = 2;

    extraOptions = let
      inherit (import ../../lib/email.nix {inherit lib;}) cock;
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
      # mail_smtppassword = cock.password; # TODO how to do this?
    };
  };

  niveum.passport.services = [
    {
      title = "Nextcloud";
      link = "https://${config.services.nextcloud.hostName}";
      description = "manages calendars, to-do lists, files, and recipes.";
    }
  ];

  services.postgresqlBackup = {
    enable = true;
    databases = [config.services.nextcloud.config.dbname];
  };

  services.postgresql = {
    enable = true;
    ensureDatabases = [config.services.nextcloud.config.dbname];
    ensureUsers = [
      {
        name = "nextcloud";
        ensurePermissions."DATABASE ${config.services.nextcloud.config.dbname}" = "ALL PRIVILEGES";
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
