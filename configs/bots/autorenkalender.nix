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

}
