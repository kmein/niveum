{
  pkgs,
  config,
  ...
}: let
  storageBoxMountPoint = "/mnt/storagebox";
in {
  # https://docs.hetzner.com/de/robot/storage-box/access/access-samba-cifs/
  fileSystems.${storageBoxMountPoint} = {
    device = "//u359050.your-storagebox.de/backup";
    fsType = "cifs";
    options = [
      "iocharset=utf8"
      "rw"
      "credentials=${config.age.secrets.hetzner-storagebox-credentials.path}"
      "uid=nextcloud"
      "gid=nextcloud"
      "file_mode=0660"
      "dir_mode=0770"
      "seal"
      "mfsymlinks" # nextcloud-setup wants to create symlinks on cifs
    ];
  };

  systemd.services.nextcloud-setup = {
    wants = ["mnt-storagebox.mount" "postgresql.service"];
    after = ["mnt-storagebox.mount" "postgresql.service"];
  };

  age.secrets = {
    hetzner-storagebox-credentials = {
      file = ../../secrets/hetzner-storagebox-credentials.age;
    };
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
    package = pkgs.nextcloud29;

    https = true;

    autoUpdateApps = {
      enable = true;
      startAt = "05:00:00";
    };

    hostName = "cloud.kmein.de";

    datadir = "${storageBoxMountPoint}/nextcloud";

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

    extraOptions = {
      defaultapp = "files";
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
        ensureDBOwnership = true;
        # ensurePermissions."DATABASE ${config.services.nextcloud.config.dbname}" = "ALL PRIVILEGES";
      }
    ];
    package = pkgs.postgresql_14;
  };

  services.nginx.virtualHosts."cloud.kmein.de" = {
    enableACME = true;
    forceSSL = true;
  };
}
