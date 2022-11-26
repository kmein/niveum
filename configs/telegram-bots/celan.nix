{
  pkgs,
  lib,
  ...
}: let
  celan = pkgs.fetchzip {
    url = "http://c.krebsco.de/celan.tar.gz";
    sha256 = "sha256-nA+EwAH2vkeolsy9AoPLEMt1uGKDZe/aPrS95CZvuus=";
  };
in {
  niveum.telegramBots.celan = {
    enable = true;
    time = "08:00";
    token = lib.strings.fileContents <system-secrets/telegram/kmein.token>;
    chatIds = ["@PaulCelan"];
    command = toString (pkgs.writers.writeDash "random-celan" ''
      cd ${celan}
      poem="$(${pkgs.findutils}/bin/find . -type f | ${pkgs.coreutils}/bin/shuf -n1)"
      source="$(${pkgs.coreutils}/bin/dirname "$poem" | ${pkgs.gnused}/bin/sed 's#^\./##;s/[-_]/ /g;s!/! › !g;s/0\([0-9]\+\)/\1/g')"
      cat "$poem"
      echo
      printf "Aus: %s\n" "$source"
    '');
  };

  systemd.timers.telegram-bot-celan.timerConfig.RandomizedDelaySec = "10h";

  niveum.passport.services = [
    {
      title = "Paul Celan Bot";
      description = "sends a random poem by Paul Celan to Telegram.";
      link = "https://t.me/PaulCelan";
    }
  ];
}
