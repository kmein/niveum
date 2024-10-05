{
  pkgs,
  config,
  lib,
  niveumPackages,
  ...
}: {
  niveum.bots.quotebot = {
    enable = true;
    time = "08/6:00";
    telegram = {
      enable = true;
      tokenFile = config.age.secrets.telegram-token-kmein.path;
      chatIds = ["-1001760262519"];
      parseMode = "Markdown";
    };
    command = "${niveumPackages.literature-quote}/bin/literature-quote";
  };

  niveum.passport.services = [
    {
      title = "Literature quote bot";
      description = "sends me and my friends three <a href=\"https://logotheca.xn--kiern-0qa.de/\">logotheca</a> quotes a day.";
    }
  ];
}
