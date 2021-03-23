{ lib, pkgs, config, ... }:
let
  inherit (import <niveum/lib>) nixpkgs-unstable tmpfilesConfig;
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

  mpc-lyrikline = pkgs.writers.writeDashBin "mpc-lyrikline" ''MPD_PORT=${toString lyrikline.mpdPort} ${pkgs.mpc_cli}/bin/mpc "$@"'';
  mpc-meddl = pkgs.writers.writeDashBin "mpc-meddl" ''MPD_PORT=${toString meddl.mpdPort} ${pkgs.mpc_cli}/bin/mpc "$@"'';
  mpc-lyrik = pkgs.writers.writeDashBin "mpc-lyrik" ''MPD_PORT=${toString lyrik.mpdPort} ${pkgs.mpc_cli}/bin/mpc "$@"'';
in
{
  imports = [ <stockholm/krebs/3modules/htgen.nix> ];
  nixpkgs.overlays = [
    (self: super: { htgen = super.callPackage <stockholm/krebs/5pkgs/simple/htgen> {}; })
  ];

  systemd.tmpfiles.rules = [
    (tmpfilesConfig {
      type = "d";
      path = radioStore;
      mode = "0755";
      user = config.users.extraUsers.radio.name;
      age = "1d";
    })
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
        "GET /lyrik/status")
          printf 'HTTP/1.1 200 OK\r\n'
          printf 'Content-Type: text/html; charset=UTF-8\r\n'
          printf 'Connection: close\r\n'
          printf '\r\n'

          video_id="$(
            MPD_PORT=${toString lyrik.mpdPort} ${pkgs.mpc_cli}/bin/mpc status -f %file% \
              | head -n1 \
              | grep -o 'id=[^&]*' \
              | sed 's/^id=//g'
          )"

          ${pkgs.youtube-dl}/bin/youtube-dl -j "https://www.youtube.com/watch?v=$video_id" \
            | ${pkgs.jq}/bin/jq -r '"% [\(.title)](\(.webpage_url))\n\n\(.description)"' \
            | sed 's/$/  /g' \
            | ${nixpkgs-unstable.pandoc}/bin/pandoc -s

          exit
        ;;
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
        "POST /meddl/skip")
          printf 'HTTP/1.1 200 OK\r\n'
          printf 'Content-Type: text/html; charset=UTF-8\r\n'
          printf 'Connection: close\r\n'
          printf '\r\n'
          ${mpc-meddl}/bin/mpc-meddl next
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
    serviceConfig.User = config.users.extraUsers.radio.name;
    preStart = "${mpc-lyrikline}/bin/mpc-lyrikline crop || :";
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

        ${mpc-lyrikline}/bin/mpc-lyrikline add "$poem_file"
      done

      ${mpc-lyrikline}/bin/mpc-lyrikline play
    '';
  };

  systemd.services.lyrik = {
    after = [ "container@lyrik.service" ];
    wantedBy = [ "container@lyrik.service" ];
    preStart = "${mpc-lyrik}/bin/mpc-lyrik crop || :";
    restartIfChanged = true;
    serviceConfig.User = config.users.extraUsers.radio.name;
    script =
      let
        videoIds = import <niveum/lib/hot-rotation/lyrik.nix>;
        streams = lib.concatMapStringsSep "\n" (id: "https://au.ytprivate.com/latest_version?id=${id}&itag=251") videoIds;
        streamsFile = pkgs.writeText "hotrot" streams;
      in ''
        set -efu
        ${mpc-lyrik}/bin/mpc-lyrik add < ${toString streamsFile}

        ${mpc-lyrik}/bin/mpc-lyrik crossfade 5
        ${mpc-lyrik}/bin/mpc-lyrik random on
        ${mpc-lyrik}/bin/mpc-lyrik repeat on
        ${mpc-lyrik}/bin/mpc-lyrik play
      '';
  };


  systemd.services.meddl = {
    after = [ "container@meddl.service" ];
    wantedBy = [ "container@meddl.service" ];
    startAt = "*:00/10";
    serviceConfig.User = config.users.extraUsers.radio.name;
    preStart = "${mpc-meddl}/bin/mpc-meddl crop || :";
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

          ${mpc-meddl}/bin/mpc-meddl add "$song_url"
        done

      ${mpc-meddl}/bin/mpc-meddl play
    '';
  };

  environment.systemPackages = [ mpc-lyrikline mpc-lyrik mpc-meddl ];

  services.nginx.virtualHosts."radio.xn--kiern-0qa.de".locations = {
    "= /meddl/status".proxyPass = "http://127.0.0.1:${toString htgenPort}";
    "= /meddl/listen.ogg".proxyPass = "http://127.0.0.1:${toString meddl.streamPort}";
    "= /meddl/skip".proxyPass = "http://127.0.0.1:${toString htgenPort}";
    "= /lyrikline/status".proxyPass = "http://127.0.0.1:${toString htgenPort}";
    "= /lyrikline/listen.ogg".proxyPass = "http://127.0.0.1:${toString lyrikline.streamPort}";
    "= /lyrik/status".proxyPass = "http://127.0.0.1:${toString htgenPort}";
    "= /lyrik/listen.ogg".proxyPass = "http://127.0.0.1:${toString lyrik.streamPort}";
    "= /lyrik.ogg".return = "301 http://radio.xn--kiern-0qa.de/lyrik/listen.ogg";
    "= /meddl.ogg".return = "301 http://radio.xn--kiern-0qa.de/meddl/listen.ogg";
    "= /lyrikline.ogg".return = "301 http://radio.xn--kiern-0qa.de/lyrikline/listen.ogg";
  };
}
