{
  pkgs,
  lib,
  config,
  ...
}: let
  celan = pkgs.fetchzip {
    url = "http://c.krebsco.de/celan.tar.gz";
    sha256 = "sha256-nA+EwAH2vkeolsy9AoPLEMt1uGKDZe/aPrS95CZvuus=";
  };
in {
  niveum.bots.celan = {
    enable = true;
    time = "08:00";
    telegram = {
      enable = true;
      tokenFile = config.age.secrets.telegram-token-kmein.path;
      chatIds = ["@PaulCelan"];
    };
    mastodon = {
      enable = true;
      tokenFile = config.age.secrets.mastodon-token-celan.path;
      language = "de";
    };
    command = toString (pkgs.writers.writeDash "random-celan" ''
      cd ${celan}
      poem="$(${pkgs.findutils}/bin/find . -type f | ${pkgs.coreutils}/bin/shuf -n1)"
      source="$(${pkgs.coreutils}/bin/dirname "$poem" \
        | ${pkgs.gnused}/bin/sed 's#^\./##;s/[-_]/ /g;s!/! › !g;s/0\([0-9]\+\)/\1/g' \
        | ${pkgs.gnused}/bin/sed 's/Der Sand aus den Urnen/#Der_Sand_aus_den_Urnen (1948)/g' \
        | ${pkgs.gnused}/bin/sed 's/Mohn und Gedächtnis/#Mohn_und_Gedächtnis (1952)/g' \
        | ${pkgs.gnused}/bin/sed 's/Von Schwelle zu Schwelle/#Von_Schwelle_zu_Schwelle (1955)/g' \
        | ${pkgs.gnused}/bin/sed 's/Sprachgitter/#& (1959)/g' \
        | ${pkgs.gnused}/bin/sed 's/Niemandsrose/#& (1963)/g' \
        | ${pkgs.gnused}/bin/sed 's/Atemwende/#& (1967)/g' \
        | ${pkgs.gnused}/bin/sed 's/Fadensonnen/#& (1968)/g' \
        | ${pkgs.gnused}/bin/sed 's/Lichtzwang/#& (1970)/g' \
        | ${pkgs.gnused}/bin/sed 's/Schneepart/#& (1971)/g' \
        | ${pkgs.gnused}/bin/sed 's/Zeitgehöft/#& (1976)/g' \
        | ${pkgs.gnused}/bin/sed 's/Frühwerk/#& (1989)/g' \
        | ${pkgs.gnused}/bin/sed 's/Eingedunkelt/#& (1991)/g' \
        | ${pkgs.gnused}/bin/sed 's/Nachlaß/#& (1997)/g'
      )"
      cat "$poem"
      echo
      printf "Aus: %s\n\n#PaulCelan #Celan #Lyrik #poetry" "$source"
    '');
  };

  age.secrets = {
    mastodon-token-celan.file = ../../secrets/mastodon-token-celan.age;
  };

  systemd.timers.bot-celan.timerConfig.RandomizedDelaySec = "10h";

  niveum.passport.services = [
    {
      title = "Paul Celan Bot";
      description = "sends a random poem by Paul Celan to Telegram.";
      link = "https://t.me/PaulCelan";
    }
  ];
}
