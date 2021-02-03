{ lib, pkgs, config, ... }:
let
  radioStore = "/var/lib/radio";
  htgenPort = 8080;
  meddl = { streamPort = 8000; mpdPort = 6600; };
  lyrikline = { streamPort = 8001; mpdPort = 6601; };
  lyrik = { streamPort = 8002; mpdPort = 6602; };
  mpd-add-with-tags = pkgs.writers.writeHaskell "mpd-add-with-tags" {
    libraries = with pkgs.haskellPackages; [ optparse-generic libmpd ];
  } ''
    {-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
    import Control.Monad (void)
    import Data.String
    import Network.MPD
    import Options.Generic

    data Options = Options { url :: String, artist :: Maybe String, title :: Maybe String }
      deriving (Generic)

    instance ParseRecord Options

    main :: IO ()
    main = do
      options <- getRecord "Add to MPD with tags"
      void $ withMPD $ do
        songId <- addId (fromString $ url options) Nothing
        maybe (pure ()) (addTagId songId Artist . fromString) $ artist options
        maybe (pure ()) (addTagId songId Title . fromString) $ title options
  '';
in
{
  imports = [ <stockholm/krebs/3modules/htgen.nix> ];
  nixpkgs.overlays = [
    (self: super: { htgen = super.callPackage <stockholm/krebs/5pkgs/simple/htgen> {}; })
  ];

  systemd.tmpfiles.rules = [
    "d '${radioStore}' 0755 ${config.users.extraUsers.radio.name} - 1d -"
  ];

  users.extraUsers.radio.isSystemUser = true;

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

  krebs.htgen.radio = {
    port = htgenPort;
    user.name = "radio";
    script = ''. ${pkgs.writers.writeDash "meinskript" ''
      case "$Method $Request_URI" in
        "GET /lyrikline/status")
          printf 'HTTP/1.1 200 OK\r\n'
          printf 'Content-Type: text/html; charset=UTF-8\r\n'
          printf 'Connection: close\r\n'
          printf '\r\n'

          hash="$(
            MPD_PORT=${toString lyrikline.mpdPort} ${pkgs.mpc_cli}/bin/mpc status -f '%file%' \
              | head -n 1 \
              | md5sum \
              | cut -d' ' -f 1
          )"
          url="$(cat ${radioStore}/$hash)"

          echo "<html><body style='margin:0'><iframe style='width:100%;height:100%;border:0' src="$url"></iframe></body></html>"
          exit
        ;;
        "GET /meddl/status")
          printf 'HTTP/1.1 200 OK\r\n'
          printf 'Content-Type: text/html; charset=UTF-8\r\n'
          printf 'Connection: close\r\n'
          printf '\r\n'

          hash="$(
            MPD_PORT=${toString meddl.mpdPort} ${pkgs.mpc_cli}/bin/mpc status -f '%file%' \
              | head -n 1 \
              | md5sum \
              | cut -d' ' -f 1
          )"
          url="$(cat ${radioStore}/$hash)"

          echo "<html><body style='margin:0'><iframe style='width:100%;height:100%;border:0' src="$url"></iframe></body></html>"
          exit
        ;;
      esac
    ''}'';
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
    environment = {
      MPD_PORT = toString lyrikline.mpdPort;
      MPD_HOST = "127.0.0.1";
    };
    serviceConfig.User = config.users.extraUsers.radio.name;
    preStart = "${pkgs.mpc_cli}/bin/mpc crop";
    script = ''
      set -efu

      lyrikline=https://www.lyrikline.org
      for _ in $(seq 1 10); do
        random_route="$(${pkgs.curl}/bin/curl -sSL "$lyrikline/index.php/tools/getrandompoem" --data-raw 'lang=de' --compressed | ${pkgs.jq}/bin/jq -r .link)"
        poem_url="$lyrikline$random_route"

        poem_file="$(
          ${pkgs.curl}/bin/curl -sSL "$poem_url" \
            | grep -o 'https://.*\.mp3' \
            | head -n1
        )"

        hash="$(echo "$poem_file" | md5sum | cut -d' ' -f 1)"
        echo "$poem_file ($hash) -> $poem_url"
        echo "$poem_url" > "${radioStore}/$hash"

        ${pkgs.mpc_cli}/bin/mpc add "$poem_file"
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
    serviceConfig.User = config.users.extraUsers.radio.name;
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
    serviceConfig.User = config.users.extraUsers.radio.name;
    preStart = "${pkgs.mpc_cli}/bin/mpc crop";
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

          hash="$(echo "$song_url" | md5sum | cut -d' ' -f 1)"
          echo "$song_url ($hash) -> $song"
          echo "$song" > "${radioStore}/$hash"

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
    "= /meddl/status".proxyPass = "http://127.0.0.1:${toString htgenPort}";
    "= /meddl/listen.ogg".proxyPass = "http://127.0.0.1:${toString meddl.streamPort}";
    "= /lyrikline/status".proxyPass = "http://127.0.0.1:${toString htgenPort}";
    "= /lyrikline/listen.ogg".proxyPass = "http://127.0.0.1:${toString lyrikline.streamPort}";
    "= /lyrik.ogg".proxyPass = "http://127.0.0.1:${toString lyrik.streamPort}";
    "= /meddl.ogg".return = "301 http://radio.xn--kiern-0qa.de/meddl/listen.ogg";
    "= /lyrikline.ogg".return = "301 http://radio.xn--kiern-0qa.de/lyrikline/listen.ogg";
  };
}
