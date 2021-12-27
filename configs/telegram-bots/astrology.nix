{ pkgs, lib, ... }:
let
  nixpkgs-unstable = import <nixpkgs-unstable> {};
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
  niveum.telegramBots.transits = {
    enable = true;
    time = "4:00";
    token = lib.strings.fileContents <system-secrets/telegram/kmein.token>;
    chatIds = [ "18980945" ];
    command = toString (pkgs.writers.writeDash "common-transits" ''
      {
        ${nixpkgs-unstable.astrolog}/bin/astrolog -n -zN Berlin -Yt -Yd -d -R Uranus Neptune Pluto
        ${nixpkgs-unstable.astrolog}/bin/astrolog -Yt -Yd -q 10 22 1999 6:32 -zN Kassel -td $(${pkgs.coreutils}/bin/date +'%m %d %Y') -R Uranus Neptune Pluto
      } | ${toSymbols} | ${pkgs.coreutils}/bin/sort -n
    '');
  };
}
