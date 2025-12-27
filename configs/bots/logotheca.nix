{
  pkgs,
  config,
  ...
}:
{
  niveum.bots.logotheca = {
    enable = true;
    time = "08/6:00";
    telegram = {
      enable = true;
      tokenFile = config.age.secrets.telegram-token-kmein.path;
      chatIds = [ "-1001760262519" ];
      parseMode = "Markdown";
    };
    matrix = {
      enable = true;
      homeserver = "matrix.4d2.org";
      tokenFile = config.age.secrets.matrix-token-lakai.path;
      chatIds = [
        "!zlwCuPiCNMSxDviFzA:4d2.org"
      ];
    };
    command = "${pkgs.literature-quote}/bin/literature-quote";
  };

  age.secrets = {
    matrix-token-lakai.file = ../../secrets/matrix-token-lakai.age;
  };

  niveum.passport.services = [
    {
      title = "Literature quote bot";
      description = "sends me and my friends three <a href=\"https://logotheca.xn--kiern-0qa.de/\">logotheca</a> quotes a day.";
    }
  ];
}
