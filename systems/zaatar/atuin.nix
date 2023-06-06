{
  config,
  pkgs,
  ...
}: let
  inherit (import ../../lib) tmpfilesConfig;
in {
  services.postgresql = {
    dataDir = "/var/state/postgresql/${config.services.postgresql.package.psqlSchema}";
    package = pkgs.postgresql_11;
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

  services.atuin = {
    host = "0.0.0.0";
    openFirewall = true;
    openRegistration = true;
    port = 8888;
  };
}
