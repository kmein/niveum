{
  pkgs,
  lib,
  ...
}: let
  hesychius = <scripts> + "/hesychius/hesychius.txt";
in {
  niveum.telegramBots.hesychius = {
    enable = true;
    time = "08:00";
    token = lib.strings.fileContents <system-secrets/telegram/kmein.token>;
    chatIds = ["@HesychiosAlexandreus"];
    command = "${pkgs.coreutils}/bin/shuf -n1 ${hesychius}";
  };

  niveum.passport.services = [
    {
      title = "Hesychius of Alexandria Bot";
      description = "sends a random word from Hesychius of Alexandria's lexicon to Telegram.";
      link = "https://t.me/HesychiosAlexandreus";
    }
  ];
}
