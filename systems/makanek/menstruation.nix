{
  config,
  pkgs,
  lib,
  ...
}:
let
  backendPort = 8000;
in
{
  services.redis.servers.menstruation = {
    enable = true;
    port = 6379;
  };

  environment.systemPackages = [ pkgs.redis ];

  niveum.passport.services = [
    {
      title = "Tischlein, deck dich!";
      description = "serves you with Berlin canteen menus via Telegram.";
      link = "https://t.me/TischleinDeckDichBot";
    }
  ];

  systemd.services.menstruation-telegram = {
    wants = [
      "network-online.target"
      "menstruation-backend.service"
      "redis-menstruation.service"
    ];
    wantedBy = [ "multi-user.target" ];
    environment = {
      MENSTRUATION_ENDPOINT = "http://localhost:${toString backendPort}";
      MENSTRUATION_MODERATORS = "18980945";
    };
    script = ''
      set -efu
      export MENSTRUATION_TOKEN="$(cat "$CREDENTIALS_DIRECTORY/menstruation-token")"
      ${pkgs.menstruation-telegram}/bin/menstruation-telegram
    '';
    serviceConfig = {
      Restart = "always";
      DynamicUser = true;
      LoadCredential = [
        "menstruation-token:${config.age.secrets.telegram-token-menstruation.path}"
      ];
    };
  };

  age.secrets.telegram-token-menstruation.file = ../../secrets/telegram-token-menstruation.age;

  systemd.services.menstruation-backend = {
    wants = [ "network-online.target" ];
    environment.ROCKET_PORT = toString backendPort;
    serviceConfig = {
      Restart = "always";
      DynamicUser = true;
      ExecStart = "${pkgs.menstruation-backend}/bin/menstruation_server";
    };
  };
}
