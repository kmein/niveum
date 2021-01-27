{ config, pkgs, lib, ... }:
let
  streams = import <niveum/lib/streams.nix> {
    di-fm-key = lib.strings.fileContents <secrets/di.fm/key>;
  };
  multi-room-audio-port = 8000;
in
{
  imports = [
    <niveum/modules/mpd-fm.nix>
  ];

  services.mpd = {
    enable = true;
    extraConfig = ''
      log_level "default"
      auto_update "yes"

      audio_output {
        type "alsa"
        name "zaatar single room audio system"
      }

      audio_output {
        type "httpd"
        name "zaatar multi room audio system"
        encoder "vorbis" # optional
        port "${toString multi-room-audio-port}"
        quality "5.0" # do not define if bitrate is defined
        # bitrate "128" # do not define if quality is defined
        format "44100:16:2"
        always_on "yes" # prevent MPD from disconnecting all listeners when playback is stopped.
        tags "yes" # httpd supports sending tags to listening streams.
      }
    '';
  };

  environment.systemPackages = [ pkgs.mpc_cli ];

  services.mpd-fm = {
    enable = true;
    stationsFile = "/etc/mpd-fm/stations.json";
    webPort = 8080;
  };

  systemd.services.mpd-fm-stations =
  let
    stations = lib.lists.imap0 (id: {desc ? "", logo ? "https://picsum.photos/seed/${builtins.hashString "md5" stream}/300", stream, station}: { inherit id desc logo stream station; }) streams;
    stationsJson = pkgs.writeText "stations.json" (builtins.toJSON stations);
  in {
    wantedBy = [ "mpd-fm.service" ];
    startAt = "hourly";
    script = ''
      mkdir -p /etc/mpd-fm
      antenne_asb_url=$(
        ${pkgs.curl}/bin/curl -sS 'https://www.caster.fm/widgets/em_player.php?jsinit=true&uid=529295&t=blue&c=' \
          | grep streamUrl \
          | sed ${lib.escapeShellArg "s/^.*'\\([^']*\\)'.*/\\1/"}
      )
      ${pkgs.jq}/bin/jq "map(if .station == \"Antenne ASB\" then .stream |= \"$antenne_asb_url\" else . end)" < ${stationsJson} > /etc/mpd-fm/stations.json
    '';
  };

  networking.firewall.allowedTCPPorts = [ 80 ];
  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    virtualHosts.default = {
      basicAuth.dj = lib.strings.fileContents <system-secrets/mpd-web.key>;
      locations."~ ^/listen" = {
        proxyPass = "http://127.0.0.1:${toString multi-room-audio-port}";
      };
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString config.services.mpd-fm.webPort}";
        proxyWebsockets = true;
      };
    };
  };
}
