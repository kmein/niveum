{ lib, pkgs, config, ... }:
let
  inherit (import <niveum/lib>) tmpfilesConfig serveHtml;

  radioStore = "/var/lib/radio";
  htgenPort = 8080;
  stations = {
    lyrikline = {
      streamPort = 8001;
      mpdPort = 6601;
      description = ''
        Weltklang. Welt als ewiges Gedicht, das seine Schallspuren durch Raum und Zeit jagt. Endlose Zufallswiedergabe von <a href="//lyrikline.org">lyrikline</a>. — Listen to the sound of voices and poems permeating linguistic and geographic barriers, 24 hours per day.
      '';
    };
    lyrik = {
      streamPort = 8002;
      mpdPort = 6602;
      description = ''
        Deutsche Lyrik, die du noch nicht gut genug kennst. Tritt in einen Fluss aus Reim und Maß; keine zwei Mal ist er derselbe.
      '';
    };
  };
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

  mpcs = lib.mapAttrs (name: station: pkgs.writers.writeDashBin "mpc-${name}" ''
    MPD_PORT=${toString station.mpdPort} ${pkgs.mpc_cli}/bin/mpc "$@"
  '') stations;
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

  users.extraUsers.radio = {
    isSystemUser = true;
    group = "radio";
  };
  users.groups.radio = {};

  krebs.htgen.radio = {
    port = htgenPort;
    user.name = "radio";
    script = ''. ${pkgs.writers.writeDash "meinskript" ''
      send200() {
        printf 'HTTP/1.1 200 OK\r\n'
        printf 'Content-Type: text/html; charset=UTF-8\r\n'
        printf 'Connection: close\r\n'
        printf '\r\n'
      }

      case "$Method $Request_URI" in
        "GET /lyrik/status")
          send200
          video_id="$(
            ${mpcs.lyrik}/bin/mpc-lyrik status -f %file% \
              | head -n1 \
              | grep -o 'id=[^&]*' \
              | sed 's/^id=//g'
          )"

          ${pkgs.youtube-dl}/bin/youtube-dl -j "https://www.youtube.com/watch?v=$video_id" \
            | ${pkgs.jq}/bin/jq -r '"% [\(.title)](\(.webpage_url))\n\n\(.description)"' \
            | sed 's/$/  /g' \
            | ${pkgs.pandoc}/bin/pandoc -s

          exit
        ;;
        "GET /lyrikline/status")
          send200

          hash="$(
            ${mpcs.lyrikline}/bin/mpc-lyrikline status -f '%file%' \
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

  containers = lib.mapAttrs (name: station: {
    autoStart = true;
    config = {config, pkgs, ...}: {
      services.mpd = {
        enable = true;
        network.port = station.mpdPort;
        extraConfig = ''
          log_level "default"

          audio_output {
            name "${name}"
            type "httpd"
            encoder "vorbis"
            port "${toString station.streamPort}"
            bitrate "128"
            format "44100:16:2"
            always_on "yes"
            tags "yes"
          }
        '';
      };
    };
  }) stations;

  environment.systemPackages = lib.attrValues mpcs;

  systemd.services.lyrikline = {
    after = [ "container@lyrikline.service" ];
    wantedBy = [ "container@lyrikline.service" ];
    startAt = "*:00/5";
    serviceConfig.User = config.users.extraUsers.radio.name;
    preStart = "${mpcs.lyrikline}/bin/mpc-lyrikline crop || :";
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

        ${mpcs.lyrikline}/bin/mpc-lyrikline add "$poem_file"
      done

      ${mpcs.lyrikline}/bin/mpc-lyrikline play
    '';
  };

  systemd.services.lyrik = {
    after = [ "container@lyrik.service" ];
    wantedBy = [ "container@lyrik.service" ];
    preStart = "${mpcs.lyrik}/bin/mpc-lyrik crop || :";
    restartIfChanged = true;
    serviceConfig.User = config.users.extraUsers.radio.name;
    script =
      let
        invidious = "https://invidious.silkky.cloud";
        videoIds = import <niveum/lib/hot-rotation/lyrik.nix>;
        streams = lib.concatMapStringsSep "\n" (id: "${invidious}/latest_version?id=${id}&itag=251") videoIds;
        streamsFile = pkgs.writeText "hotrot" streams;
      in ''
        set -efu
        ${mpcs.lyrik}/bin/mpc-lyrik add < ${toString streamsFile}

        ${mpcs.lyrik}/bin/mpc-lyrik crossfade 5
        ${mpcs.lyrik}/bin/mpc-lyrik random on
        ${mpcs.lyrik}/bin/mpc-lyrik repeat on
        ${mpcs.lyrik}/bin/mpc-lyrik play
      '';
  };


  services.nginx.virtualHosts."radio.kmein.de" = {
    enableACME = true;
    forceSSL = true;
    locations = lib.mkMerge (
      [
        { "/".extraConfig = serveHtml (import ./station-list.nix { inherit pkgs lib stations; }) pkgs; }
      ] ++ (lib.mapAttrsToList (name: station: {
        "= /${name}/status".proxyPass = "http://127.0.0.1:${toString htgenPort}";
        "= /${name}/listen.ogg".proxyPass = "http://127.0.0.1:${toString station.streamPort}";
        "= /${name}.ogg".return = "301 http://radio.xn--kiern-0qa.de/${name}/listen.ogg"; # legacy
      }) stations)
    );
  };
}
