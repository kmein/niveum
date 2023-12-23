{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  telebots = inputs.telebots.defaultPackage.x86_64-linux;
  reverseDirectory = "/run/telegram-reverse";
  proverbDirectory = "/run/telegram-proverb";
  inherit (import ../../lib) tmpfilesConfig;
in {
  imports = [
    ./literature-quote.nix
    ./astrology.nix
    ./autorenkalender.nix
    ./hesychius.nix
    ./smyth.nix
    ./nachtischsatan.nix
    ./tlg-wotd.nix
    ./celan.nix
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

  age.secrets = {
    telegram-token-reverse.file = ../../secrets/telegram-token-reverse.age;
    telegram-token-betacode.file = ../../secrets/telegram-token-betacode.age;
    telegram-token-proverb.file = ../../secrets/telegram-token-proverb.age;
    telegram-token-streaming-link.file = ../../secrets/telegram-token-streaming-link.age;
  };

  systemd.services.telegram-reverse = {
    wantedBy = ["multi-user.target"];
    description = "Telegram reverse bot";
    path = [pkgs.ffmpeg];
    enable = true;
    script = ''
      TELEGRAM_BOT_TOKEN="$(cat "$CREDENTIALS_DIRECTORY/token")" ${telebots}/bin/telegram-reverse
    '';
    serviceConfig.Restart = "always";
    serviceConfig.WorkingDirectory = reverseDirectory;
    serviceConfig.LoadCredential = "token:${config.age.secrets.telegram-token-reverse.path}";
  };

  systemd.services.telegram-streaming-link = {
    wantedBy = ["multi-user.target"];
    description = "Telegram bot converting YouTube Music <-> Spotify";
    enable = true;
    script = ''
      TELEGRAM_BOT_TOKEN="$(cat "$CREDENTIALS_DIRECTORY/token")" ${telebots}/bin/telegram-streaming-link
    '';
    serviceConfig.Restart = "always";
    serviceConfig.LoadCredential = "token:${config.age.secrets.telegram-token-streaming-link.path}";
  };

  systemd.services.telegram-betacode = {
    wantedBy = ["multi-user.target"];
    description = "Telegram beta code bot";
    enable = true;
    script = ''
      TELEGRAM_BOT_TOKEN="$(cat "$CREDENTIALS_DIRECTORY/token")" ${telebots}/bin/telegram-betacode
    '';
    serviceConfig.Restart = "always";
    serviceConfig.LoadCredential = "token:${config.age.secrets.telegram-token-betacode.path}";
  };

  systemd.services.telegram-proverb = {
    wantedBy = ["multi-user.target"];
    description = "Telegram proverb bot";
    enable = true;
    script = ''
      TELEGRAM_BOT_TOKEN="$(cat "$CREDENTIALS_DIRECTORY/token")" ${telebots}/bin/telegram-proverb
    '';
    serviceConfig.Restart = "always";
    serviceConfig.WorkingDirectory = proverbDirectory;
    serviceConfig.LoadCredential = "token:${config.age.secrets.telegram-token-proverb.path}";
  };
}
