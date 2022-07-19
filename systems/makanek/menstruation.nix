{
  pkgs,
  lib,
  ...
}: let
  backend = pkgs.callPackage <menstruation-backend> {};
  telegram = pkgs.callPackage <menstruation-telegram> {};
  backendPort = 8000;
in {
  services.redis.servers.menstruation = {
    enable = true;
    port = 6379;
  };

  environment.systemPackages = [pkgs.redis];

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
    wantedBy = ["multi-user.target"];
    environment = {
      MENSTRUATION_TOKEN = lib.strings.fileContents <system-secrets/telegram/menstruation.token>;
      MENSTRUATION_ENDPOINT = "http://localhost:${toString backendPort}";
      MENSTRUATION_MODERATORS = "18980945";
    };
    serviceConfig = {
      Restart = "always";
      DynamicUser = true;
      ExecStart = "${telegram}/bin/menstruation-telegram";
    };
  };

  systemd.services.menstruation-backend = {
    wants = ["network-online.target"];
    environment.ROCKET_PORT = toString backendPort;
    serviceConfig = {
      Restart = "always";
      DynamicUser = true;
      ExecStart = "${backend}/bin/menstruation_server";
    };
  };
}
