{ config, pkgs, lib, ... }:
let
  # telegram-horoscope = pkgs.python3Packages.callPackage <packages/telegram-horoscope.nix> {};
in {
  nixpkgs.config.packageOverrides = pkgs: {
    autorenkalender = pkgs.callPackage <packages/autorenkalender.nix> {};
    literature-quote = pkgs.callPackage <packages/literature-quote.nix> {};
    telegram-proverb = pkgs.python3Packages.callPackage <packages/telegram-proverb.nix> {};
    telegram-betacode = pkgs.python3Packages.callPackage <packages/telegram-betacode.nix> {};
    telegram-reverse = pkgs.python3Packages.callPackage <packages/telegram-reverse.nix> {};
  };

  niveum.telegramBots.quotebot = {
    enable = true;
    time = "08:00";
    token = lib.strings.removeSuffix "\n" (builtins.readFile <secrets/telegram-kmein.token>);
    chatIds = [ "18980945" ];
    command = "${pkgs.literature-quote}/bin/literature-quote";
    parseMode = "Markdown";
  };

  niveum.telegramBots.autorenkalender = {
    enable = true;
    time = "07:00";
    token = lib.strings.removeSuffix "\n" (builtins.readFile <secrets/telegram-kmein.token>);
    chatIds = [ "@autorenkalender" ];
    command = "${pkgs.autorenkalender}/bin/autorenkalender";
  };

  systemd.services.telegram-reverse = {
    wantedBy = [ "multi-user.target" ];
    description = "Telegram bot for reversing things";
    environment.TELEGRAM_REVERSE_TOKEN = builtins.readFile <secrets/telegram-reverse.token>;
    enable = true;
    script = ''${pkgs.telegram-reverse}/bin/telegram-reverse'';
    serviceConfig.Restart = "always";
  };

  systemd.services.telegram-betacode = {
    wantedBy = [ "multi-user.target" ];
    description = "Telegram bot for converting Ancient Greek betacode into unicode";
    environment.TELEGRAM_BETACODE_TOKEN = builtins.readFile <secrets/telegram-betacode.token>;
    enable = true;
    script = ''${pkgs.telegram-betacode}/bin/telegram-betacode'';
    serviceConfig.Restart = "always";
  };

  systemd.services.telegram-proverb = {
    wantedBy = [ "multi-user.target" ];
    description = "Telegram bot for generating inspiring but useless proverbs";
    environment.TELEGRAM_PROVERB_TOKEN = builtins.readFile <secrets/telegram-proverb.token>;
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
