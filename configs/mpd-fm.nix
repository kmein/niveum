{ config, pkgs, lib, ... }:
let
  firewall = (import <niveum/lib>).firewall lib;

  streams = import <niveum/lib/streams.nix> {
    di-fm-key = lib.strings.fileContents <secrets/di.fm/key>;
  };
  multi-room-audio-port = 8000;
  password = lib.strings.fileContents <system-secrets/mpd-web.key>;
in
{
  imports = [
    <niveum/modules/mpd-fm.nix>
  ];

  services.mpd = {
    enable = true;
    network.listenAddress = "0.0.0.0";
    extraConfig = ''
      log_level "default"
      auto_update "yes"

      audio_output {
        type "pulse"
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

  networking.firewall =
  let
    dport = config.services.mpd.network.port;
    protocol = "tcp";
    rules = [
      (firewall.accept { inherit dport protocol; source = "192.168.0.0/16"; })
      (firewall.accept { inherit dport protocol; source = "127.0.0.0/8"; })
    ];
  in {
    allowedTCPPorts = [ 80 ];
    extraCommands = firewall.addRules rules;
    extraStopCommands = firewall.removeRules rules;
  };

  system.activationScripts.mpd-playlists =
  let playlistFile = pkgs.writeText "radio.m3u" (lib.concatMapStringsSep "\n" (lib.getAttr "stream") streams);
  in ''
    rm -rf /var/lib/mpd/playlists
    install -d /var/lib/mpd/playlists
    ln -sfn "${toString playlistFile}" "/var/lib/mpd/playlists/radio.m3u"
  '';

  services.mpd-fm = {
    enable = true;
    # stationsFile = "/etc/mpd-fm/stations.json";
    stations = lib.lists.imap0 (id: {desc ? "", logo ? "https://picsum.photos/seed/${builtins.hashString "md5" stream}/300", stream, station}: { inherit id desc logo stream station; }) streams;
    webPort = 8080;
  };

  systemd.services.mpd-fm-stations =
  let
    stations = lib.lists.imap0 (id: {desc ? "", logo ? "https://picsum.photos/seed/${builtins.hashString "md5" stream}/300", stream, station}: { inherit id desc logo stream station; }) streams;
    stationsJson = (pkgs.formats.json {}).generate "stations.json" stations;
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


  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    virtualHosts.default = {
      basicAuth.dj = password;
      locations."= /listen.ogg" = {
        proxyPass = "http://127.0.0.1:${toString multi-room-audio-port}";
      };
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString config.services.mpd-fm.webPort}";
        proxyWebsockets = true;
      };
    };
  };
}
