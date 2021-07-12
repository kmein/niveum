{ pkgs, lib }:
let
  streams-m3u = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/jnk22/kodinerds-iptv/master/iptv/kodi/kodi_tv.m3u";
    sha256 = "1fd91y87cydhxn6brisn4hkx73z6axhgr1fjamayg0scffyapir8";
  };
  streams-tsv = pkgs.runCommand "streams.tsv" {} ''
    ${pkgs.gnused}/bin/sed '/#EXTM3U/d;/#EXTINF/{s/\r\n//g; s/.*,//g}' ${streams-m3u} | ${pkgs.coreutils}/bin/paste -d'\t' - - > $out
  '';
in pkgs.writers.writeDashBin "mpv-tv" ''
  exec ${pkgs.mpv}/bin/mpv --force-window=yes "$(${pkgs.dmenu}/bin/dmenu -i -l 5 < ${streams-tsv} | ${pkgs.coreutils}/bin/cut -f2)"
''
