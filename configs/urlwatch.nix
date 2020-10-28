{ lib, ... }:
{
  imports = [ <stockholm/krebs/3modules/urlwatch.nix> ];

  krebs.urlwatch = {
    enable = true;
    onCalendar = "*-*-* 05:00:00";
    sendmail.enable = false;
    telegram = {
      enable = true;
      chatId = [ "18980945" ];
      botToken = lib.strings.fileContents <system-secrets/telegram/kmein.token>;
    };
    urls = [ ];
  };
}
