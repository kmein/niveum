{
  config,
  pkgs,
  lib,
  ...
}:
{
  niveum.bots.smyth = {
    enable = true;
    time = "08:00";
    mastodon = {
      enable = true;
      tokenFile = config.age.secrets.mastodon-token-smyth.path;
      language = "en";
    };
    telegram = {
      enable = true;
      tokenFile = config.age.secrets.telegram-token-kmein.path;
      chatIds = [ "@HerbertWeirSmyth" ];
    };
    command = toString (
      pkgs.writers.writeDash "random-smyth" ''
        set -efu

        good_curl() {
          ${pkgs.curl}/bin/curl "$@" \
            --compressed \
            -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' \
            -H 'Accept-Language: en-US,en;q=0.5' \
            -H 'DNT: 1' \
            -H 'Connection: keep-alive' \
            -H 'Upgrade-Insecure-Requests: 1' \
            -H 'Sec-Fetch-Dest: document' \
            -H 'Sec-Fetch-Mode: navigate' \
            -H 'Sec-Fetch-Site: cross-site' \
            -H 'Priority: u=0, i' \
            -H 'Pragma: no-cache' \
            -H 'Cache-Control: no-cache'
        }

        RANDOM_SECTION=$(
          good_curl -sSL http://www.perseus.tufts.edu/hopper/xmltoc?doc=Perseus%3Atext%3A1999.04.0007%3Asmythp%3D1 \
            | ${pkgs.gnugrep}/bin/grep -o 'ref="[^"]*"' \
            | ${pkgs.coreutils}/bin/shuf -n1 \
            | ${pkgs.gnused}/bin/sed 's/^ref="//;s/"$//'
        )

        url="http://www.perseus.tufts.edu/hopper/text?doc=$RANDOM_SECTION"
        good_curl -sSL "$url"\
          | ${pkgs.htmlq}/bin/htmlq '#text_main' \
          | ${pkgs.gnused}/bin/sed 's/<\/\?hr>//g' \
          | ${pkgs.pandoc}/bin/pandoc -f html -t plain --wrap=none

        printf '\n%s\n\n#AncientGreek' "$url"
      ''
    );
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
