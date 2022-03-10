{pkgs, ...}: {
  environment.systemPackages = [
    (pkgs.writers.writeDashBin "gaslight-stream" ''
      ${pkgs.ffmpeg}/bin/ffmpeg -r 14 -s 640x480 -f video4linux2 -i /dev/video0 -f alsa -i default -c:v libx264 -preset ultrafast -c:a aac -f avi -
    '')
    (pkgs.writers.writeDashBin "gaslight-say" ''
      voices="de
      de+whisper"

      echo "$@" | ${pkgs.espeak}/bin/espeak -v "$(echo "$voices" | ${pkgs.coreutils}/bin/shuf -n1)"
    '')
    (pkgs.writers.writeDashBin "gaslight-play" ''
      set -o noglob
      ${pkgs.mpv}/bin/mpv --no-video "$1"
    '')
  ];
}
/*
 
 ssh machine gaslight-stream | mpv -
 ssh machine gaslight-say "blablabla"
 */

