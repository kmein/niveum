{config, ...}: let
  inherit (import <niveum/lib>) tmpfilesConfig;
  unstable = import <nixos-unstable> {inherit (config.nixpkgs) config;};
in {
  services.postgresql = {
    enable = true;
    dataDir = "/var/state/postgresql/${config.services.postgresql.package.psqlSchema}";
    ensureDatabases = ["atuin"];
    ensureUsers = [
      {
        name = "atuin";
        ensurePermissions."DATABASE atuin" = "ALL PRIVILEGES";
      }
    ];
  };

  services.postgresqlBackup = {
    enable = true;
    databases = ["atuin"];
  };

  systemd.tmpfiles.rules = [
    (tmpfilesConfig {
      type = "d";
      path = "/var/state/postgresql";
      mode = "0700";
      user = "postgres";
      group = "postgres";
    })
  ];

  users.groups.atuin = {};
  users.users.atuin = {
    isSystemUser = true;
    group = "atuin";
    home = "/run/atuin";
    createHome = true;
  };

  systemd.services.atuin = {
    wantedBy = ["multi-user.target"];
    environment = {
      ATUIN_HOST = "0.0.0.0";
      ATUIN_PORT = "8888";
      ATUIN_OPEN_REGISTRATION = "true";
      ATUIN_DB_URI = "postgres:///atuin";
    };
    serviceConfig = {
      User = "atuin";
      ExecStart = "${unstable.atuin}/bin/atuin server start";
      Restart = "on-failure";
    };
  };
  networking.firewall.allowedTCPPorts = [8888];
}
