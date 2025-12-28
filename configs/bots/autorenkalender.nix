{
  config,
  pkgs,
  ...
}:
{
  niveum.bots.autorenkalender = {
    enable = true;
    time = "07:00";
    telegram = {
      enable = true;
      tokenFile = config.age.secrets.telegram-token-kmein.path;
      chatIds = [ "@autorenkalender" ];
      parseMode = "Markdown";
    };
    command = "${pkgs.autorenkalender}/bin/autorenkalender";
  };

  niveum.passport.services = [
    {
      title = "Autorenkalender";
      description = "sends <a href=\"https://www.projekt-gutenberg.org/\">Projekt Gutenberg</a>'s anniversary information to Telegram.";
      link = "https://t.me/Autorenkalender";
    }
  ];
}
