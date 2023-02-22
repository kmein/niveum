{
  pkgs,
  config,
  inputs,
  lib,
  ...
}: let
  hesychius = inputs.scripts.outPath + "/hesychius/hesychius.txt";
in {
  niveum.telegramBots.hesychius = {
    enable = true;
    time = "08:00";
    tokenFile = config.age.secrets.telegram-token-kmein.path;
    chatIds = ["@HesychiosAlexandreus"];
    command = "${pkgs.coreutils}/bin/shuf -n1 ${hesychius}";
  };

  systemd.timers.telegram-bot-hesychius.timerConfig.RandomizedDelaySec = "10h";

  niveum.passport.services = [
    {
      title = "Hesychius of Alexandria Bot";
      description = "sends a random word from Hesychius of Alexandria's lexicon to Telegram.";
      link = "https://t.me/HesychiosAlexandreus";
    }
  ];
}
