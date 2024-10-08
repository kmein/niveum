{
  config,
  pkgs,
  lib,
  ...
}: let
  toSymbols = pkgs.writers.writeDash "to-symbols" ''
    ${pkgs.gnused}/bin/sed '
      s/\bTri\b/△/;
      s/\bOpp\b/☍/;
      s/\bSqu\b/□/;
      s/\bSex\b/⚹/;
      s/\bCon\b/☌/;
      s/Sun/☉/g;
      s/Moon/☽/g;
      s/Mercury/☿/g;
      s/Venus/♀/g;
      s/Mars/♂/g;
      s/Jupiter/♃/g;
      s/Saturn/♄/g;
      s/Uranus/♅/g;
      s/Neptune/♆/g;
      s/Pluto/♇/g;
      s/North Node/☊/g;
      s/\bLeo\b/♌/g;
      s/\bCan\(cer\)\?\b/♋/g;
      s/\bGem\(ini\)\?\b/♊/g;
      s/\bVir\(go\)\?\b/♍/g;
      s/\bLib\(ra\)\?\b/♎/g;
      s/\bTau\(rus\)\?\b/♉/g;
      s/\bAri\(es\)\?\b/♈/g;
      s/\bSco\(rpio\)\?\b/♏/g;
      s/\bPis\(ces\)\?\b/♓/g;
      s/\bSag\(ittarius\)\?\b/♐/g;
      s/\bAqu\(arius\)\?\b/♒/g;
      s/\bCap\(ricorn\)\?\b/♑/g;
      s/-->/⟶/g;
      s/\s\+/ /g; # replace multiple spaces by one
      s/^.*\([ 0-9]\{2\}:[0-9]\{2\}\) /\1 /; # remove date at beginning of line (but not everything up to v/c X:XX)
      s/^\s*//
    '
  '';
in {
  niveum.bots.transits = {
    enable = true;
    time = "*:0/1";
    mastodon = {
      enable = true;
      tokenFile = config.age.secrets.mastodon-token-transits.path;
    };
    telegram = {
      enable = true;
      tokenFile = config.age.secrets.telegram-token-kmein.path;
      chatIds = ["-1001796440545"];
    };
    command = toString (pkgs.writers.writeDash "common-transits" ''
      set -efu

      now=$(${pkgs.coreutils}/bin/date +%_H:%M | ${pkgs.gnused}/bin/sed 's/^\s*//')
      date=$(${pkgs.coreutils}/bin/date +'%m %d %Y')
      (
        cd ${pkgs.astrolog}/bin
        # ./astrolog -Yt -Yd -q 10 22 1999 6:32 -zN Kassel -td $date -R Uranus Neptune Pluto "North Node"
        ./astrolog -qd $date -zN Berlin -Yt -Yd -d -R Uranus Neptune Pluto "North Node" -A 2
      ) | ${toSymbols} | ${pkgs.coreutils}/bin/sort -n | ${pkgs.gnugrep}/bin/grep "^$now" || :
    '');
  };

  age.secrets = {
    mastodon-token-transits.file = ../../secrets/mastodon-token-transits.age;
  };
}
