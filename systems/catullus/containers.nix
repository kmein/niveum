{ config, pkgs, lib, ... }:
let
  telegram-reverse = pkgs.python3Packages.callPackage <packages/telegram-reverse.nix> {};
  telegram-betacode = pkgs.python3Packages.callPackage <packages/telegram-betacode.nix> {};
  # telegram-horoscope = pkgs.python3Packages.callPackage <packages/telegram-horoscope.nix> {};
  telegram-proverb = pkgs.python3Packages.callPackage <packages/telegram-proverb.nix> {};
  autorenkalender = pkgs.callPackage <packages/autorenkalender.nix> {};

in {
  containers.telegram-bots = {
    autoStart = true;
    config = {
      systemd.services.quotebot = {
        enable = true;
        startAt = "08:00";
        serviceConfig.Type = "oneshot";
        wantedBy = [ "multi-user.target" ];
        environment = {
          TELEGRAM_AUTORENKALENDER_CHAT = "18980945";
          TELEGRAM_AUTORENKALENDER_TOKEN = lib.strings.removeSuffix "\n" (builtins.readFile <secrets/telegram-kmein.token>);
        };
        script = ''
          ROW=$(${pkgs.curl}/bin/curl -s https://raw.githubusercontent.com/kmein/quotes/master/quotes.csv?token=ACO7O6523EG3TDCYTT2K4224XTW5W | shuf -n1)

          QUOTE=$((
            echo "$ROW" | ${pkgs.xsv}/bin/xsv select 4;
            printf '— %s: *%s*, %s\n' \
              "$(echo "$ROW" | ${pkgs.xsv}/bin/xsv select 1)" \
              "$(echo "$ROW" | ${pkgs.xsv}/bin/xsv select 2)" \
              "$(echo "$ROW" | ${pkgs.xsv}/bin/xsv select 3)"
          ) | ${pkgs.gnused}/bin/sed 's/ | /\n/g;s/"\(.*\)"/\1/')

          TELEGRAM_ENDPOINT="https://api.telegram.org/bot$TELEGRAM_AUTORENKALENDER_TOKEN"
          ${pkgs.curl}/bin/curl -s \
            -X POST "$TELEGRAM_ENDPOINT/sendMessage" \
            -d chat_id="$TELEGRAM_AUTORENKALENDER_CHAT" \
            -d parse_mode=Markdown \
            -d text="$QUOTE"
        '';
      };

      systemd.services.autorenbot = {
        enable = true;
        startAt = "07:00";
        serviceConfig.Type = "oneshot";
        wantedBy = [ "multi-user.target" ];
        environment = {
          TELEGRAM_AUTORENKALENDER_CHAT = "18980945";
          TELEGRAM_AUTORENKALENDER_TOKEN = lib.strings.removeSuffix "\n" (builtins.readFile <secrets/telegram-kmein.token>);
        };
        script = ''
          TELEGRAM_ENDPOINT="https://api.telegram.org/bot$TELEGRAM_AUTORENKALENDER_TOKEN"
          ${pkgs.curl}/bin/curl -s \
            -X POST "$TELEGRAM_ENDPOINT/sendMessage" \
            -d chat_id="$TELEGRAM_AUTORENKALENDER_CHAT" \
            -d text="$(${autorenkalender}/bin/autorenkalender)"
        '';
      };
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
      systemd.services.telegram-proverb = {
        wantedBy = [ "multi-user.target" ];
        description = "Telegram bot for generating inspiring but useless proverbs";
        environment.TELEGRAM_PROVERB_TOKEN = builtins.readFile <secrets/telegram-proverb.token>;
        enable = true;
        script = ''${telegram-proverb}/bin/proverb_bot.py'';
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
    };
  };
}
