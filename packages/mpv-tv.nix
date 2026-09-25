{
  lib,
  writers,
  fetchurl,
  gnused,
  coreutils,
  mpv,
  dmenu,
}:
let
  m3u-to-tsv = ''
    ${lib.getExe gnused} '/#EXTM3U/d;/#EXTINF/s/.*,//g' $out | ${lib.getExe' coreutils "paste"} -d'\t' - - > $out.tmp
    mv $out.tmp $out
  '';

  live-tv = fetchurl {
    url = "https://raw.githubusercontent.com/Free-TV/IPTV/39a573d7a428ca1b2ffeec422751a01d37e59e94/playlist.m3u8";
    hash = "sha256-GBJBJN1AwwtO8HYrD0y3/qPCiK48IXyjt93s6DF/7Yo=";
    postFetch = m3u-to-tsv;
  };

  kodi-tv = fetchurl {
    url = "https://raw.githubusercontent.com/jnk22/kodinerds-iptv/3f35761b7edcfb356d22cac0e561592ba589c20b/iptv/kodi/kodi_tv.m3u";
    hash = "sha256-NYWHfX36c0FHJpGeyW5VzjmrU00Nme2oF7lKafmWI5Y=";
    postFetch = m3u-to-tsv;
  };
in
writers.writeDashBin "mpv-tv" ''
  cat ${kodi-tv} ${live-tv} | ${lib.getExe mpv} --force-window=yes "$(${lib.getExe dmenu} -i -l 5 | ${lib.getExe' coreutils "cut"} -f2)"
''
