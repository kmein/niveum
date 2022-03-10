{
  pkgs,
  lib,
  ...
}: let
  nachtischsatan-bot = {token}:
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


      updater = Updater('${token}')

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
      token = lib.strings.fileContents <system-secrets/telegram/nachtischsatan.token>;
    });
    serviceConfig.Restart = "always";
  };
}
