{
  config,
  pkgs,
  lib,
  ...
}: let
  firewall = (import <niveum/lib>).firewall lib;
  inherit (import <niveum/lib>) tmpfilesConfig;

  streams = import <niveum/lib/streams.nix> {
    di-fm-key = lib.strings.fileContents <secrets/di.fm/key>;
  };
  multi-room-audio-port = 8000;
  password = lib.strings.fileContents <system-secrets/mpd-web.key>;
in {
  imports = [
    <niveum/modules/tuna.nix>
  ];

  services.syncthing = let
    mpd-directory = config.services.mpd.dataDir;
  in {
    enable = true;
    user = config.services.mpd.user; # config.users.extraUsers.moodle.name;
    openDefaultPorts = true;
    configDir = "${mpd-directory}/.config/syncthing";
    dataDir = "${mpd-directory}/.config/syncthing";
    cert = toString <system-secrets/syncthing/cert.pem>;
    key = toString <system-secrets/syncthing/key.pem>;
    devices = {
      inherit ((import <niveum/lib>).syncthing.devices) kabsa manakish heym;
    };
    folders.${config.services.mpd.musicDirectory} = {
      devices = ["heym" "kabsa" "manakish"];
      id = "music";
      type = "receiveonly";
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

  environment.systemPackages = [pkgs.mpc_cli];

  networking.firewall = let
    dport = config.services.mpd.network.port;
    protocol = "tcp";
    rules = [
      (firewall.accept {
        inherit dport protocol;
        source = "192.168.0.0/16";
      })
      (firewall.accept {
        inherit dport protocol;
        source = "127.0.0.0/8";
      })
    ];
  in {
    allowedTCPPorts = [80];
    extraCommands = firewall.addRules rules;
    extraStopCommands = firewall.removeRules rules;
  };

  systemd.tmpfiles.rules = let
    tags = lib.lists.unique (lib.concatMap ({tags ? [], ...}: tags) streams);
    tagStreams = tag: map (lib.getAttr "stream") (lib.filter ({tags ? [], ...}: lib.elem tag tags) streams);
    makePlaylist = name: urls: pkgs.writeText "${name}.m3u" (lib.concatStringsSep "\n" urls);
  in
    map (tag:
      tmpfilesConfig {
        type = "L+";
        path = "/var/lib/mpd/playlists/${tag}.m3u";
        mode = "0644";
        user = "mpd";
        group = "mpd";
        argument = makePlaylist tag (tagStreams tag);
      })
    tags
    + [
      (tmpfilesConfig {
        type = "L+";
        mode = "0644";
        user = "mpd";
        group = "mpd";
        path = "/var/lib/mpd/playlist/all.m3u";
        argument = makePlaylist "all" streams;
      })
    ];

  services.tuna = {
    enable = true;
    # stationsFile = "/etc/tuna/stations.json";
    stations = lib.lists.imap0 (id: {
      desc ? "",
      logo ? "https://picsum.photos/seed/${builtins.hashString "md5" stream}/300",
      stream,
      station,
      ...
    }: {inherit id desc logo stream station;})
    streams;
    webPort = 7044;
  };

  services.ympd = {
    enable = true;
    mpd.port = config.services.mpd.network.port;
  };

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    virtualHosts."radio.kmein.r" = {
      basicAuth.dj = password;
      locations."/" = {
        proxyPass = "http://127.0.0.1:${config.services.ympd.webPort}";
        proxyWebsockets = true;
      };
    };
  };
}
