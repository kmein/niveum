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
    mastodon = {
      enable = true;
      homeserver = "c.im";
      tokenFile = config.age.secrets.mastodon-token-logotheca.path;
      language = "de";
    };
    command = "${niveumPackages.literature-quote}/bin/literature-quote";
  };

  age.secrets = {
    mastodon-token-logotheca.file = ../../secrets/mastodon-token-logotheca.age;
  };

  niveum.passport.services = [
    {
      title = "Literature quote bot";
      description = "sends me and my friends three <a href=\"https://logotheca.xn--kiern-0qa.de/\">logotheca</a> quotes a day.";
    }
  ];
}
