{
  config,
  pkgs,
  lib,
  ...
}: {
  niveum.bots.smyth = {
    enable = true;
    time = "08:00";
    mastodon = {
      enable = true;
      tokenFile = config.age.secrets.mastodon-token-smyth.path;
      language = "en";
      tokenFile = config.age.secrets.mastodon-token-smyth.path;
    };
    telegram = {
      enable = true;
      tokenFile = config.age.secrets.telegram-token-kmein.path;
      chatIds = ["@HerbertWeirSmyth"];
    };
    command = toString (pkgs.writers.writeDash "random-smyth" ''
      set -efu

      RANDOM_SECTION=$(
        ${pkgs.curl}/bin/curl -sSL http://www.perseus.tufts.edu/hopper/xmltoc?doc=Perseus%3Atext%3A1999.04.0007%3Asmythp%3D1 \
          | ${pkgs.gnugrep}/bin/grep -o 'ref="[^"]*"' \
          | ${pkgs.coreutils}/bin/shuf -n1 \
          | ${pkgs.gnused}/bin/sed 's/^ref="//;s/"$//'
      )

      ${pkgs.curl}/bin/curl -sSL http://www.perseus.tufts.edu/hopper/text?doc=$RANDOM_SECTION\
        | ${pkgs.htmlq}/bin/htmlq '#text_main' \
        | ${pkgs.gnused}/bin/sed 's/<\/\?hr>//g' \
        | ${pkgs.pandoc}/bin/pandoc -f html -t plain --wrap=none
    '');
  };

  systemd.timers.bot-smyth.timerConfig.RandomizedDelaySec = "10h";

  age.secrets = {
    mastodon-token-smyth.file = ../../secrets/mastodon-token-smyth.age;
  };

  niveum.passport.services = [
    {
      title = "Herbert Weir Smyth Bot";
      description = "sends a random section from Smyth's Ancient Greek grammar to Telegram.";
      link = "https://t.me/HerbertWeirSmyth";
    }
  ];
}
