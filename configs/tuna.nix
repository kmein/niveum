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
    <niveum/modules/tuna.nix>
  ];

  services.syncthing = let mpd-directory = config.services.mpd.dataDir; in {
    enable = true;
    user = config.services.mpd.user; # config.users.extraUsers.moodle.name;
    openDefaultPorts = true;
    configDir = "${mpd-directory}/.config/syncthing";
    dataDir = "${mpd-directory}/.config/syncthing";
    declarative = rec {
      cert = toString <system-secrets/syncthing/cert.pem>;
      key = toString <system-secrets/syncthing/key.pem>;
      devices = {
        inherit ((import <niveum/lib>).syncthing.devices) wilde manakish heym;
      };
      folders.${config.services.mpd.musicDirectory} = {
        devices = [ "heym" "wilde" "manakish" ];
        id = "music";
        type = "receiveonly";
      };
    };
  };

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

  services.tuna = {
    enable = true;
    # stationsFile = "/etc/tuna/stations.json";
    stations = lib.lists.imap0 (id: {desc ? "", logo ? "https://picsum.photos/seed/${builtins.hashString "md5" stream}/300", stream, station}: { inherit id desc logo stream station; }) streams;
    webPort = 8080;
  };

  systemd.services.tuna-stations =
  let
    stations = lib.lists.imap0 (id: {desc ? "", logo ? "https://picsum.photos/seed/${builtins.hashString "md5" stream}/300", stream, station}: { inherit id desc logo stream station; }) streams;
    stationsJson = (pkgs.formats.json {}).generate "stations.json" stations;
  in {
    wantedBy = [ "tuna.service" ];
    startAt = "hourly";
    script = ''
      mkdir -p /etc/tuna
      antenne_asb_url=$(
        ${pkgs.curl}/bin/curl -sS 'https://www.caster.fm/widgets/em_player.php?jsinit=true&uid=529295&t=blue&c=' \
          | grep streamUrl \
          | sed ${lib.escapeShellArg "s/^.*'\\([^']*\\)'.*/\\1/"}
      )
      ${pkgs.jq}/bin/jq "map(if .station == \"Antenne ASB\" then .stream |= \"$antenne_asb_url\" else . end)" < ${stationsJson} > /etc/tuna/stations.json
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
        proxyPass = "http://127.0.0.1:${toString config.services.tuna.webPort}";
        proxyWebsockets = true;
      };
    };
  };
}
