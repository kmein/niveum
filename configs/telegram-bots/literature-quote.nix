{
  pkgs,
  config,
  lib,
  niveumPackages,
  ...
}: {
  niveum.telegramBots.quotebot = {
    enable = true;
    time = "08/6:00";
    tokenFile = config.age.secrets.telegram-token-kmein.path;
    chatIds = ["-1001760262519"];
    command = "${niveumPackages.literature-quote}/bin/literature-quote";
    parseMode = "Markdown";
  };

  niveum.passport.services = [
    {
      title = "Literature quote bot";
      description = "sends me and my friends three <a href=\"https://logotheca.xn--kiern-0qa.de/\">logotheca</a> quotes a day.";
    }
  ];
}
