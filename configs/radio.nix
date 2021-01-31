{ lib, pkgs, config, ... }:
let
  meddl = {
    streamPort = 8000;
    mpdPort = 6600;
  };
  lyrikline = {
    streamPort = 8001;
    mpdPort = 6601;
  };
  lyrik = {
    streamPort = 8002;
    mpdPort = 6602;
  };
in
{
  containers.lyrik = {
    autoStart = true;
    config = {config, pkgs, ...}: {
      services.mpd = {
        enable = true;
        network.port = lyrik.mpdPort;
        extraConfig = ''
          log_level "default"

          audio_output {
            name "Lyrik-Radio"
            type "httpd"
            encoder "vorbis"
            port "${toString lyrik.streamPort}"
            bitrate "128"
            format "44100:16:2"
            always_on "yes"
            tags "yes"
          }
        '';
      };
    };
  };

  containers.meddl = {
    autoStart = true;
    config = {config, pkgs, ...}: {
      services.mpd = {
        enable = true;
        network.port = meddl.mpdPort;
        extraConfig = ''
          log_level "default"
          volume_normalization "yes"

          audio_output {
            name "DrachenLord Radio"
            type "httpd"
            encoder "vorbis"
            port "${toString meddl.streamPort}"
            bitrate "128"
            format "44100:16:2"
            always_on "yes"
            tags "yes"
          }
        '';
      };

    };
  };

  containers.lyrikline = {
    autoStart = true;
    config = {config, pkgs, ...}: {
      services.mpd = {
        enable = true;
        network.port = lyrikline.mpdPort;
        extraConfig = ''
          log_level "default"

          audio_output {
            name "lyrikline.org Radio"
            type "httpd"
            encoder "vorbis"
            port "${toString lyrikline.streamPort}"
            bitrate "128"
            format "44100:16:2"
            always_on "yes"
            tags "yes"
          }
        '';
      };
    };
  };

  systemd.services.lyrikline = {
    after = [ "container@lyrikline.service" ];
    wantedBy = [ "container@lyrikline.service" ];
    startAt = "*:00/5";
    environment.MPD_PORT = toString lyrikline.mpdPort;
    script = ''
      set -efu

      lyrikline=https://www.lyrikline.org
      for _ in $(seq 1 10); do
        random_route="$(${pkgs.curl}/bin/curl -sSL "$lyrikline/index.php/tools/getrandompoem" --data-raw 'lang=de' --compressed | ${pkgs.jq}/bin/jq -r .link)"
        poem_url="$(${pkgs.curl}/bin/curl -sSL "$lyrikline$random_route" | grep -o 'https://.*\.mp3' | head -n1)"
        ${pkgs.mpc_cli}/bin/mpc add "$poem_url"
      done

      ${pkgs.mpc_cli}/bin/mpc play
    '';
  };

  systemd.services.lyrik = {
    after = [ "container@lyrik.service" ];
    wantedBy = [ "container@lyrik.service" ];
    environment.MPD_PORT = toString lyrik.mpdPort;
    preStart = "${pkgs.mpc_cli}/bin/mpc crop";
    restartIfChanged = true;
    script =
      let
        videoIds = import <niveum/lib/hot-rotation/lyrik.nix>;
        streams = lib.concatMapStringsSep "\n" (id: "https://au.ytprivate.com/latest_version?id=${id}&itag=251") videoIds;
        streamsFile = pkgs.writeText "hotrot" streams;
      in ''
        set -efu

        ${pkgs.mpc_cli}/bin/mpc add < ${toString streamsFile}

        ${pkgs.mpc_cli}/bin/mpc crossfade 5
        ${pkgs.mpc_cli}/bin/mpc random on
        ${pkgs.mpc_cli}/bin/mpc repeat on
        ${pkgs.mpc_cli}/bin/mpc play
      '';
  };


  systemd.services.meddl = {
    after = [ "container@meddl.service" ];
    wantedBy = [ "container@meddl.service" ];
    startAt = "*:00/10";
    environment.MPD_PORT = toString meddl.mpdPort;
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

  environment.systemPackages = [
    (pkgs.writers.writeDashBin "mpc-lyrikline" ''
      MPD_PORT=${toString lyrikline.mpdPort} ${pkgs.mpc_cli}/bin/mpc "$@"
    '')
    (pkgs.writers.writeDashBin "mpc-meddl" ''
      MPD_PORT=${toString meddl.mpdPort} ${pkgs.mpc_cli}/bin/mpc "$@"
    '')
    (pkgs.writers.writeDashBin "mpc-lyrik" ''
      MPD_PORT=${toString lyrik.mpdPort} ${pkgs.mpc_cli}/bin/mpc "$@"
    '')
  ];

  services.nginx.virtualHosts."radio.xn--kiern-0qa.de".locations = {
    "= /meddl.ogg".proxyPass = "http://127.0.0.1:${toString meddl.streamPort}";
    "= /lyrikline.ogg".proxyPass = "http://127.0.0.1:${toString lyrikline.streamPort}";
    "= /lyrik.ogg".proxyPass = "http://127.0.0.1:${toString lyrik.streamPort}";
  };
}
