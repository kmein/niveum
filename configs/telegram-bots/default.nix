{
  config,
  pkgs,
  lib,
  ...
}: let
  nixpkgs-21-11 = import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-21.11.tar.gz") {};
  telebots = nixpkgs-21-11.callPackage <telebots> {};
  reverseDirectory = "/run/telegram-reverse";
  proverbDirectory = "/run/telegram-proverb";
  inherit (import <niveum/lib>) tmpfilesConfig;
in {
  imports = [
    ./literature-quote.nix
    ./astrology.nix
    ./autorenkalender.nix
    ./nachtischsatan.nix
    ./tlg-wotd.nix
    <niveum/modules/telegram-bot.nix>
  ];

  systemd.tmpfiles.rules = map (path:
    tmpfilesConfig {
      type = "d";
      mode = "0750";
      age = "1h";
      inherit path;
    }) [reverseDirectory proverbDirectory];

  niveum.passport.services = [
    {
      title = "Rückwarts-Bot";
      link = "https://t.me/RueckwaertsBot";
      description = "reverses things on Telegram.";
    }
    {
      title = "BetaCode-Bot";
      link = "https://t.me/BetaCodeBot";
      description = "converts <a href=\"https://en.wikipedia.org/wiki/Beta_Code\">beta code</a> to polytonic Greek on Telegram.";
    }
    {
      title = "Sprichwortgenerator-Bot";
      link = "https://t.me/SprichwortGeneratorBot";
      description = "generates useless German proverbs with optional stock photo background on Telegram.";
    }
  ];

  systemd.services.telegram-reverse = {
    wantedBy = ["multi-user.target"];
    description = "Telegram reverse bot";
    path = [pkgs.ffmpeg];
    environment.TELEGRAM_BOT_TOKEN = lib.strings.fileContents <system-secrets/telegram/reverse.token>;
    enable = true;
    script = "${telebots}/bin/telegram-reverse";
    serviceConfig.Restart = "always";
    serviceConfig.WorkingDirectory = reverseDirectory;
  };

  systemd.services.telegram-betacode = {
    wantedBy = ["multi-user.target"];
    description = "Telegram beta code bot";
    environment.TELEGRAM_BOT_TOKEN = lib.strings.fileContents <system-secrets/telegram/betacode.token>;
    enable = true;
    script = "${telebots}/bin/telegram-betacode";
    serviceConfig.Restart = "always";
  };

  systemd.services.telegram-proverb = {
    wantedBy = ["multi-user.target"];
    description = "Telegram proverb bot";
    environment.TELEGRAM_BOT_TOKEN = lib.strings.fileContents <system-secrets/telegram/proverb.token>;
    enable = true;
    script = "${telebots}/bin/telegram-proverb";
    serviceConfig.Restart = "always";
    serviceConfig.WorkingDirectory = proverbDirectory;
  };
}
