{
  pkgs,
  config,
  ...
}:
let
  storageBoxMountPoint = "/mnt/storagebox";
  domain = "cloud.${pkgs.lib.niveum.domain}";
in
{
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
    wants = [
      "mnt-storagebox.mount"
      "postgresql.service"
    ];
    after = [
      "mnt-storagebox.mount"
      "postgresql.service"
    ];
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
    # same file again, for the exporter: the entry above is nextcloud:nextcloud 440
    # and the exporter runs as its own user
    nextcloud-password-admin-exporter = {
      file = ../../secrets/nextcloud-password-admin.age;
      owner = "nextcloud-exporter";
      mode = "400";
    };
  };

  services.nextcloud = {
    enable = true;
    package = pkgs.nextcloud33;

    https = true;

    autoUpdateApps = {
      enable = true;
      startAt = "05:00:00";
    };

    hostName = domain;

    home = "${storageBoxMountPoint}/nextcloud";

    phpOptions."opcache.interned_strings_buffer" = "32"; # buffer size in MB

    config = {
      dbtype = "pgsql";
      dbuser = "nextcloud";
      dbhost = "/run/postgresql"; # nextcloud will add /.s.PGSQL.5432 by itself
      dbname = "nextcloud";
      dbpassFile = config.age.secrets.nextcloud-password-database.path;
      adminpassFile = config.age.secrets.nextcloud-password-admin.path;
      adminuser = "admin";
      # extraTrustedDomains = [ "toum.r" ];
    };

    settings = {
      defaultapp = "files";
      overwriteprotocol = "https";
      default_phone_region = "DE";
      log_level = 2;
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
    databases = [ config.services.nextcloud.config.dbname ];
  };

  services.postgresql = {
    enable = true;
    ensureDatabases = [ config.services.nextcloud.config.dbname ];
    ensureUsers = [
      {
        name = "nextcloud";
        ensureDBOwnership = true;
        # ensurePermissions."DATABASE ${config.services.nextcloud.config.dbname}" = "ALL PRIVILEGES";
      }
    ];
    package = pkgs.postgresql_14;
  };

  services.nginx.virtualHosts.${domain} = {
    enableACME = true;
    forceSSL = true;
  };

  services.prometheus.exporters = {
    postgres = {
      enable = true;
      listenAddress = "127.0.0.1";
      runAsLocalSuperUser = true; # peer auth over /run/postgresql, no credentials
    };

    php-fpm = {
      enable = true;
      listenAddress = "127.0.0.1";
    };

    nextcloud = {
      enable = true;
      listenAddress = "127.0.0.1";
      url = "https://${domain}";
      username = config.services.nextcloud.config.adminuser;
      passwordFile = config.age.secrets.nextcloud-password-admin-exporter.path;
      timeout = "20s";
    };
  };

  systemd.services.prometheus-php-fpm-exporter = {
    # the exporter speaks FastCGI rather than HTTP, so it talks to the pool socket
    # directly; the module passes no --phpfpm.scrape-uri, this env var is the way
    environment.PHP_FPM_SCRAPE_URI = "unix://${config.services.phpfpm.pools.nextcloud.socket};/status";
    serviceConfig = {
      SupplementaryGroups = [ config.services.nginx.group ]; # socket is nginx:nginx 0660
      RestrictAddressFamilies = [ "AF_UNIX" ]; # merged with the module's AF_INET/AF_INET6
    };
  };
}
