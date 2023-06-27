{
  pkgs,
  config,
  inputs,
  ...
}: let
  nachtischsatan-bot = {tokenFile}:
    pkgs.writers.writePython3 "nachtischsatan-bot" {
      libraries = [pkgs.python3Packages.python-telegram-bot];
    } ''
      from telegram.ext import Application, ContextTypes, MessageHandler, filters
      from telegram import Update
      import random
      import time


      async def flubber(update: Update, context: ContextTypes.DEFAULT_TYPE):
          time.sleep(random.randrange(4000) / 1000)
          await update.message.reply_text("*flubberflubber*")


      with open('${tokenFile}', 'r') as tokenFile:
          token = tokenFile.read().strip()
          application = Application.builder().token(token).build()
          application.add_handler(MessageHandler(filters.ALL, flubber))
          application.run_polling()
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

  age.secrets.telegram-token-nachtischsatan.file = inputs.secrets + "/telegram-token-nachtischsatan.age";

  niveum.passport.services = [
    {
      title = "Nachtischsatan-Bot";
      link = "https://t.me/NachtischsatanBot";
      description = "*flubberflubber*";
    }
  ];
}
