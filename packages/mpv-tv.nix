{
  pkgs,
  lib,
}: let
  m3u-to-tsv = ''
    ${pkgs.gnused}/bin/sed '/#EXTM3U/d;/#EXTINF/s/.*,//g' $out | ${pkgs.coreutils}/bin/paste -d'\t' - - > $out.tmp
    mv $out.tmp $out
  '';

  live-tv = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/Free-TV/IPTV/master/playlist.m3u8";
    hash = "sha256-l8tBwdTWm7l4h/eIHlcflDTgvv6lGMs6Vt7eVMpNLlw=";
    postFetch = m3u-to-tsv;
  };

  kodi-tv = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/jnk22/kodinerds-iptv/master/iptv/kodi/kodi_tv.m3u";
    sha256 = "sha256-EZEshHWUejLTy6qsBhELfaYdDpQ/uqPsZa1JA0mb7h0=";
    postFetch = m3u-to-tsv;
  };
in
  pkgs.writers.writeDashBin "mpv-tv" ''
    cat ${kodi-tv} ${live-tv} | ${pkgs.mpv}/bin/mpv --force-window=yes "$(${pkgs.dmenu}/bin/dmenu -i -l 5 | ${pkgs.coreutils}/bin/cut -f2)"
  ''
