{ pkgs, config, inputs, ... }:
let
  postgrestPort = 3001;
  alewPort = 3000;
in
{
  systemd.services.postgrest = {
    enable = true;
    wantedBy = ["podman-alew.service"];
    wants = ["postgresql.service"];
    environment = {
      PGRST_DB_ANON_ROLE = "alew_1";
      PGRST_DB_SCHEMA = "alew_2022_05"; # alew_2023_09 for most recent (beta)
      PGRST_DB_URI = "postgres://alew_1:alew_1@localhost:5432/alew";
      PGRST_SERVER_PORT = toString postgrestPort;
    };
    script = "${pkgs.postgrest}/bin/postgrest";
    serviceConfig = {
      User = "postgres";
      Group = "postgres";
    };
  };

  services.nginx.virtualHosts."alew.kmein.de" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://localhost:${toString alewPort}";
    };
  };

  # deploy nuxt app using
  # $ cd alew/web/
  # $ rsync -rav --delete . ful:/var/lib/alew --exclude .git --exclude .nuxt --exclude node_modules
  systemd.services.alew = {
    enable = true;
    wantedBy = ["multi-user.target"];
    wants = ["postgrest.service"];
    path = [
      pkgs.yarn
      pkgs.python3
      pkgs.gcc
      pkgs.bash
      pkgs.coreutils
      pkgs.gnumake
    ];
    environment = {
      NODE_OPTIONS= "--openssl-legacy-provider";
      POSTGREST_URL = "http://localhost:${toString postgrestPort}";
      BETA = "0";
      PORT = toString alewPort;
    };
    preStart = ''
      # rsync -rav --delete ALEW_SRC .
      yarn install
      yarn build
    '';
    script = "yarn start";
    serviceConfig = {
      User = "alew";
      Group = "alew";
      WorkingDirectory = config.users.extraUsers.alew.home;
    };
  };

  users.extraUsers.alew = {
    isSystemUser = true;
    createHome = true;
    home = "/var/lib/alew";
    group = "alew";
  };

  users.extraGroups.alew = {};

  environment.systemPackages = [pkgs.podman];

  services.postgresql = {
    enable = true;
    ensureDatabases = ["alew"];
    ensureUsers = [
      {
        name = "alew";
        ensureDBOwnership = true;
      }
      { name = "alew_1"; } # readonly
    ];
    package = pkgs.postgresql_16;
  };
}
