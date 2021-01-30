{ pkgs, config, ... }:
let
  meddlPort = 8000;
in
{
  containers.meddl = {
    autoStart = true;
    config = {config, pkgs, ...}: {
      environment.systemPackages = [ pkgs.mpc_cli ];
      systemd.services.meddl = {
        before = [ "mpd.service" ];
        wantedBy = [ "mpd.service" ];
        startAt = "*:00/10";
        script = ''
          set -efu
          host=http://antenne-asb.ga

          prepend_host() {
            sed "s#^#$host/#"
          }

          ${pkgs.curl}/bin/curl -sSL "$host" \
            | ${pkgs.pup}/bin/pup 'li a attr{href}' \
            | prepend_host \
            | while read -r song; do
              song_url="$(${pkgs.curl}/bin/curl -sSL "$song" \
                | ${pkgs.pup}/bin/pup 'audio source attr{src}' \
                | prepend_host
              )"
              ${pkgs.mpc_cli}/bin/mpc add "$song_url"
            done

          ${pkgs.mpc_cli}/bin/mpc play
        '';
      };
      services.mpd = {
        enable = true;
        extraConfig = ''
          log_level "default"
          volume_normalization "yes"

          audio_output {
            name "DrachenLord Radio"
            type "httpd"
            encoder "vorbis"
            port "${toString meddlPort}"
            bitrate "128"
            format "44100:16:2"
            always_on "yes"
            tags "yes"
          }
        '';
      };

    };
  };

  services.nginx.virtualHosts."makanek.r".locations = {
    "= /meddl.ogg".proxyPass = "http://127.0.0.1:${toString meddlPort}";
  };
}
