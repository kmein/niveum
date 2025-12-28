{
  pkgs,
  config,
  ...
}:
{
  niveum.bots.hesychius = {
    enable = true;
    time = "08:00";
    mastodon = {
      enable = true;
      language = "el";
      tokenFile = config.age.secrets.mastodon-token-hesychius.path;
    };
    telegram = {
      enable = true;
      tokenFile = config.age.secrets.telegram-token-kmein.path;
      chatIds = [ "@HesychiosAlexandreus" ];
    };
    command = "${pkgs.coreutils}/bin/shuf -n1 ${pkgs.hesychius}";
  };

  systemd.timers.bot-hesychius.timerConfig.RandomizedDelaySec = "10h";

  age.secrets = {
    mastodon-token-hesychius.file = ../../secrets/mastodon-token-hesychius.age;
  };

  niveum.passport.services = [
    {
      title = "Hesychius of Alexandria Bot";
      description = "sends a random word from Hesychius of Alexandria's lexicon to Telegram.";
      link = "https://t.me/HesychiosAlexandreus";
    }
  ];
}
