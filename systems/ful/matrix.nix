{
  config,
  pkgs,
  ...
}:
let
  # retiolum only and never federating: the server name is permanent, and being
  # the host itself spares clients a .well-known lookup
  domain = "chat.kmein.r";
  admin = "@kmein:${domain}";
  synapse = "http://127.0.0.1:8008";
  telegramPort = 29317;
  whatsappPort = 29318;
in
{
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_17;
    # synapse refuses databases not collated as C
    initialScript = pkgs.writeText "synapse-init.sql" ''
      CREATE ROLE "matrix-synapse" LOGIN;
      CREATE DATABASE "matrix-synapse" WITH OWNER "matrix-synapse"
        TEMPLATE template0 LC_COLLATE = "C" LC_CTYPE = "C";
    '';
  };

  services.postgresqlBackup = {
    enable = true;
    databases = [ "matrix-synapse" ];
  };

  services.matrix-synapse = {
    enable = true;
    settings = {
      server_name = domain;
      # no public_baseurl: it defaults to the same url, and setting it makes the
      # login response point clients there, away from e.g. an ssh tunnel
      # synapse writes it on first start; matrix-synapse-register_new_matrix_user reads it
      registration_shared_secret_path = "${config.services.matrix-synapse.dataDir}/registration_shared_secret";
      federation_domain_whitelist = [ ];
      trusted_key_servers = [ ];
      presence.enabled = false;
      # the bridges invite us to one room per chat; without federation every
      # inviter is a local user, i.e. us or a bridge
      auto_accept_invites = {
        enabled = true;
        only_from_local_users = true;
      };
      # a fresh bridge login invites us to dozens of rooms at once, and the
      # default limit lets the auto-accept above give up on most of them
      rc_joins.local = {
        per_second = 10;
        burst_count = 100;
      };
      database.name = "psycopg2";
      listeners = [
        {
          port = 8008;
          bind_addresses = [ "127.0.0.1" ];
          type = "http";
          tls = false;
          x_forwarded = true;
          resources = [
            {
              names = [ "client" ];
              compress = true;
            }
          ];
        }
      ];
    };
  };

  services.mautrix-whatsapp = {
    enable = true;
    # libolm is marked insecure; goolm is the pure go replacement
    package = pkgs.mautrix-whatsapp.override { withGoolm = true; };
    settings = {
      homeserver.address = synapse;
      appservice = {
        hostname = "127.0.0.1";
        port = whatsappPort;
        address = "http://127.0.0.1:${toString whatsappPort}";
      };
      bridge.permissions.${admin} = "admin";
    };
  };

  services.mautrix-telegram = {
    enable = true;
    # without end-to-bridge encryption it needs no libolm
    package = pkgs.mautrix-telegram.override { withE2BE = false; };
    # MAUTRIX_TELEGRAM_TELEGRAM_API_ID and MAUTRIX_TELEGRAM_TELEGRAM_API_HASH, from my.telegram.org
    environmentFile = config.age.secrets.mautrix-telegram.path;
    settings = {
      homeserver = {
        address = synapse;
        inherit domain;
      };
      appservice = {
        hostname = "127.0.0.1";
        port = telegramPort;
        address = "http://127.0.0.1:${toString telegramPort}";
      };
      bridge = {
        permissions.${admin} = "admin";
        # by default it bridges only the 15 most recent chats, and no dms
        startup_sync = true;
        sync_create_limit = 0;
        sync_direct_chats = true;
      };
    };
  };

  age.secrets.mautrix-telegram.file = ../../secrets/mautrix-telegram.age;

  services.nginx.virtualHosts.${domain} = {
    extraConfig = ''
      # synapse's default max_upload_size
      client_max_body_size 50M;
    ''
    + pkgs.lib.niveum.nginxAllow pkgs.lib.niveum.ownAddresses;
    locations."/_matrix".proxyPass = synapse;
    locations."/_synapse/client".proxyPass = synapse;
  };

  services.restic.backups.niveum.paths = [
    config.services.postgresqlBackup.location
    config.services.matrix-synapse.dataDir
    "/var/lib/mautrix-whatsapp"
    "/var/lib/mautrix-telegram"
  ];
}
