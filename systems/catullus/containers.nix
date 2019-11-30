{ config, pkgs, lib, ... }:
let
  telebots-package = pkgs.fetchFromGitHub {
    owner = "kmein";
    repo = "telebots";
    rev = "b4276155114ee96cd3f320e361e52952ea700db6";
    sha256 = "08rp1pcisk4zzhxdlgrlhxa0sbza5qhxa70rjycg4r7fmixkkbz2";
  };
  proverb-bot-package = pkgs.fetchFromGitHub {
    owner = "kmein";
    repo = "proverb-pro";
    rev = "f4201c5419354377a26b7f7873368683efbea417";
    sha256 = "1ixffmxy3sxy2if7fd44ps451rds14hnz4d0x9nkh8lzshqk6v4y";
  };
in {
  nixpkgs.overlays = [(self: super: {
    autorenkalender = super.callPackage <niveum/packages/scripts/autorenkalender.nix> {};
    literature-quote = super.callPackage <niveum/packages/scripts/literature-quote.nix> {};
    telegram-proverb = super.python3Packages.callPackage proverb-bot-package {};
    telegram-reverse = super.python3Packages.callPackage "${telebots-package}/telegram-reverse" {};
    telegram-odyssey = super.python3Packages.callPackage "${telebots-package}/telegram-odyssey" {};
    telegram-betacode = super.python3Packages.callPackage "${telebots-package}/telegram-betacode" {};
  })];

  niveum.telegramBots.quotebot = {
    enable = true;
    time = "08/6:00";
    token = lib.strings.fileContents <secrets/telegram/kmein.token>;
    chatIds = [ "18980945" "757821027" "455964311" ];
    command = "${pkgs.literature-quote}/bin/literature-quote";
    parseMode = "Markdown";
  };

  niveum.telegramBots.autorenkalender = {
    enable = true;
    time = "07:00";
    token = lib.strings.fileContents <secrets/telegram/kmein.token>;
    chatIds = [ "@autorenkalender" ];
    command = "${pkgs.autorenkalender}/bin/autorenkalender";
  };

  systemd.services.telegram-odyssey = {
    wantedBy = [ "multi-user.target" ];
    description = "Telegram bot reciting the Odyssey to you";
    environment.TELEGRAM_ODYSSEY_TOKEN = lib.strings.fileContents <secrets/telegram/odyssey.token>;
    enable = true;
    script = ''${pkgs.telegram-odyssey}/bin/telegram-odyssey'';
    serviceConfig.Restart = "always";
  };

  systemd.services.telegram-reverse = {
    wantedBy = [ "multi-user.target" ];
    description = "Telegram bot for reversing things";
    environment.TELEGRAM_REVERSE_TOKEN = lib.strings.fileContents <secrets/telegram/reverse.token>;
    enable = true;
    script = ''${pkgs.telegram-reverse}/bin/telegram-reverse'';
    serviceConfig.Restart = "always";
  };

  systemd.services.telegram-betacode = {
    wantedBy = [ "multi-user.target" ];
    description = "Telegram bot for converting Ancient Greek betacode into unicode";
    environment.TELEGRAM_BETACODE_TOKEN = lib.strings.fileContents <secrets/telegram/betacode.token>;
    enable = true;
    script = ''${pkgs.telegram-betacode}/bin/telegram-betacode'';
    serviceConfig.Restart = "always";
  };

  systemd.services.telegram-proverb = {
    wantedBy = [ "multi-user.target" ];
    description = "Telegram bot for generating inspiring but useless proverbs";
    environment.TELEGRAM_PROVERB_TOKEN = lib.strings.fileContents <secrets/telegram/proverb.token>;
    enable = true;
    script = ''${pkgs.telegram-proverb}/bin/proverb_bot.py'';
    serviceConfig.Restart = "always";
  };

  # systemd.services.telegram-horoscope = {
  #   wantedBy = [ "multi-user.target" ];
  #   description = "Telegram bot for generating horoscope charts";
  #   environment.TELEGRAM_HOROSCOPE_TOKEN = builtins.readFile <secrets/telegram-horoscope.token>;
  #   environment.GOOGLE_MAPS_API_KEY = builtins.readFile <secrets/google-maps.api-key>;
  #   enable = true;
  #   script = ''${telegram-horoscope}/bin/telegram-horoscope'';
  #   serviceConfig.Restart = "always";
  # };
}
