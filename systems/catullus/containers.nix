{ config, pkgs, ... }:
let
  telegram-reverse = pkgs.python3Packages.callPackage <niveum/packages/telegram-reverse.nix> {};
  telegram-betacode = pkgs.python3Packages.callPackage <niveum/packages/telegram-betacode.nix> {};
  telegram-horoscope = pkgs.python3Packages.callPackage <niveum/packages/telegram-horoscope.nix> {};
in {
  containers.telegram-bots = {
    autoStart = true;
    config = {
      systemd.services.telegram-reverse = {
        wantedBy = [ "multi-user.target" ];
        description = "Telegram bot for reversing things";
        environment.TELEGRAM_REVERSE_TOKEN = builtins.readFile <secrets/telegram-reverse.token>;
        enable = true;
        script = ''${telegram-reverse}/bin/telegram-reverse'';
        serviceConfig.Restart = "always";
      };
      systemd.services.telegram-betacode = {
        wantedBy = [ "multi-user.target" ];
        description = "Telegram bot for converting Ancient Greek betacode into unicode";
        environment.TELEGRAM_BETACODE_TOKEN = builtins.readFile <secrets/telegram-betacode.token>;
        enable = true;
        script = ''${telegram-betacode}/bin/telegram-betacode'';
        serviceConfig.Restart = "always";
      };
      systemd.services.telegram-horoscope = {
        wantedBy = [ "multi-user.target" ];
        description = "Telegram bot for generating horoscope charts";
        environment.TELEGRAM_HOROSCOPE_TOKEN = builtins.readFile <secrets/telegram-horoscope.token>;
        environment.GOOGLE_MAPS_API_KEY = builtins.readFile <secrets/google-maps.api-key>;
        enable = true;
        script = ''${telegram-horoscope}/bin/telegram-horoscope'';
        serviceConfig.Restart = "always";
      };
    };
  };
}
