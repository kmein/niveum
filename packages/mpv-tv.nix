{
  pkgs,
  lib,
}: let
  streams-tsv = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/jnk22/kodinerds-iptv/master/iptv/kodi/kodi_tv.m3u";
    sha256 = "sha256-EZEshHWUejLTy6qsBhELfaYdDpQ/uqPsZa1JA0mb7h0=";
    postFetch = ''
      ${pkgs.gnused}/bin/sed '/#EXTM3U/d;/#EXTINF/s/.*,//g' $out | ${pkgs.coreutils}/bin/paste -d'\t' - - > $out.tmp
      mv $out.tmp $out
    '';
  };
in
  pkgs.writers.writeDashBin "mpv-tv" ''
    exec ${pkgs.mpv}/bin/mpv --force-window=yes "$(${pkgs.dmenu}/bin/dmenu -i -l 5 < ${streams-tsv} | ${pkgs.coreutils}/bin/cut -f2)"
  ''
