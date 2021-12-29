{ pkgs, lib, ... }:
let
  backend = pkgs.callPackage <niveum/submodules/menstruation-backend> {};
  telegram = pkgs.callPackage <niveum/submodules/menstruation-telegram> {};
  backendPort = 8000;
in
{
  containers.menstruation.autoStart = true;
  containers.menstruation.config = { config, pkgs, ... }: {
    services.redis.enable = true;

    systemd.services.menstruation-telegram = {
      wants = [
        "network-online.target"
        "menstruation-backend.service"
        "redis.service"
      ];
      wantedBy = [ "multi-user.target" ];
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
      wants = [ "network-online.target" ];
      environment.ROCKET_PORT = toString backendPort;
      serviceConfig = {
        Restart = "always";
        DynamicUser = true;
        ExecStart = "${backend}/bin/menstruation_server";
      };
    };
  };
}
