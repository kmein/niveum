{
  pkgs,
  config,
  lib,
  ...
}:
let
  nachtischsatan-bot =
    pkgs.writers.writePython3 "nachtischsatan-bot"
      {
        libraries = [ pkgs.python3Packages.python-telegram-bot ];
      }
      ''
        from telegram.ext import Application, ContextTypes, MessageHandler, filters
        from telegram import Update
        import os
        import random
        import time


        async def flubber(update: Update, context: ContextTypes.DEFAULT_TYPE):
            time.sleep(random.randrange(4000) / 1000)
            await update.message.reply_text("*flubberflubber*")


        token_path = os.path.join(os.environ["CREDENTIALS_DIRECTORY"], "token")
        with open(token_path, 'r') as tokenFile:
            token = tokenFile.read().strip()
            application = Application.builder().token(token).build()
            application.add_handler(MessageHandler(filters.ALL, flubber))
            application.run_polling()
      '';
in
{
  systemd.services.telegram-nachtischsatan = {
    wantedBy = [ "multi-user.target" ];
    description = "*flubberflubber*";
    enable = true;
    serviceConfig = pkgs.lib.niveum.hardening // {
      ExecStart = nachtischsatan-bot;
      Restart = "always";
      DynamicUser = true;
      LoadCredential = "token:${config.age.secrets.telegram-token-nachtischsatan.path}";
    };
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
