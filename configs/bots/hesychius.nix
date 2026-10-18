{
  pkgs,
  config,
  ...
}:
{
  niveum.bots.hesychius = {
    enable = true;
    time = "08:00";
    telegram = {
      enable = true;
      tokenFile = config.age.secrets.telegram-token-kmein.path;
      chatIds = [ "@HesychiosAlexandreus" ];
    };
    command = "${pkgs.coreutils}/bin/shuf -n1 ${pkgs.hesychius}";
  };

  systemd.timers.bot-hesychius.timerConfig.RandomizedDelaySec = "10h";

}
