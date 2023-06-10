{pkgs, ...}: {
  services.postgresqlBackup = {
    enable = true;
    databases = ["atuin"];
  };

  services.postgresql.package = pkgs.postgresql_14;

  services.atuin = {
    host = "0.0.0.0";
    openFirewall = true;
    openRegistration = true;
    port = 8888;
    enable = true;
  };
}
