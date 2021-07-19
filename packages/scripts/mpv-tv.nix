{ pkgs, lib }:
let
  streams-tsv = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/jnk22/kodinerds-iptv/master/iptv/kodi/kodi_tv.m3u";
    sha256 = "1cw1gmb16vwh6qfw0z3wjjfgn0zg2qplnddqzva5b0xx2g2appla";
    postFetch = ''
      ${pkgs.gnused}/bin/sed '/#EXTM3U/d;/#EXTINF/s/.*,//g' $out | ${pkgs.coreutils}/bin/paste -d'\t' - - > $out.tmp
      mv $out.tmp $out
    '';
  };
in pkgs.writers.writeDashBin "mpv-tv" ''
  exec ${pkgs.mpv}/bin/mpv --force-window=yes "$(${pkgs.dmenu}/bin/dmenu -i -l 5 < ${streams-tsv} | ${pkgs.coreutils}/bin/cut -f2)"
''
