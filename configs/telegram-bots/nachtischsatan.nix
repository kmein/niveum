{
  pkgs,
  config,
  lib,
  ...
}: let
  nachtischsatan-bot = {tokenFile}:
    pkgs.writers.writePython3 "nachtischsatan-bot" {
      libraries = [pkgs.python3Packages.python-telegram-bot];
    } ''
      from telegram.ext import Updater, MessageHandler
      from telegram.ext.filters import Filters
      import random
      import time


      def flubber(update, context):
          time.sleep(random.randrange(4000) / 1000)
          update.message.reply_text("*flubberflubber*")


      with open('${tokenFile}', 'r') as tokenFile:
          updater = Updater(tokenFile.read().strip())

          updater.dispatcher.add_handler(MessageHandler(Filters.all, flubber))
          updater.start_polling()
          updater.idle()
    '';
in {
  systemd.services.telegram-nachtischsatan = {
    wantedBy = ["multi-user.target"];
    description = "*flubberflubber*";
    enable = true;
    script = toString (nachtischsatan-bot {
      tokenFile = config.age.secrets.telegram-token-nachtischsatan.path;
    });
    serviceConfig.Restart = "always";
  };

  age.secrets.telegram-token-nachtischsatan.file = ../../secrets/telegram-token-nachtischsatan.age;

  niveum.passport.services = [
    {
      title = "Nachtischsatan-Bot";
      link = "https://t.me/NachtischsatanBot";
      description = "*flubberflubber*";
    }
  ];
}
