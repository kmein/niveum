{
  config,
  pkgs,
  lib,
  ...
}: let
  firewall = (import ../../lib).firewall lib;
  inherit (import ../../lib) tmpfilesConfig;

  streams = import ../../lib/streams.nix {
    di-fm-key = ""; # TODO lib.strings.fileContents <secrets/di.fm/key>;
  };
  multi-room-audio-port = 8000;
in {
  services.syncthing = let
    mpd-directory = config.services.mpd.dataDir;
  in {
    enable = true;
    user = config.services.mpd.user; # config.users.extraUsers.moodle.name;
    openDefaultPorts = true;
    configDir = "${mpd-directory}/.config/syncthing";
    dataDir = "${mpd-directory}/.config/syncthing";
    cert = config.age.secrets.syncthing-cert.path;
    key = config.age.secrets.syncthing-key.path;
    devices = {
      inherit ((import ../../lib).syncthing.devices) kabsa manakish heym;
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

  # to reset:
  # ssh zaatar 'rm /var/lib/mpd/playlists/*.m3u && systemd-tmpfiles --create'
  systemd.tmpfiles.rules = let
    tags = lib.lists.unique (lib.concatMap ({tags ? [], ...}: tags) streams);
    tagStreams = tag: lib.filter ({tags ? [], ...}: lib.elem tag tags) streams;
    makePlaylist = name: streams: pkgs.writeText "${name}.m3u" (lib.concatMapStringsSep "\n" (lib.getAttr "stream") streams);
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
    ++ [
      (tmpfilesConfig {
        type = "L+";
        mode = "0644";
        user = "mpd";
        group = "mpd";
        path = "/var/lib/mpd/playlists/all.m3u";
        argument = makePlaylist "all" streams;
      })
    ];

  services.ympd = {
    enable = true;
    mpd.port = config.services.mpd.network.port;
  };

  age.secrets = {
    ympd-basicAuth = {
      file = ../../secrets/zaatar-ympd-basicAuth.age;
      owner = "nginx";
      group = "nginx";
      mode = "400";
    };
    syncthing-cert.file = ../../secrets/zaatar-syncthing-cert.age;
    syncthing-key.file = ../../secrets/zaatar-syncthing-key.age;
    di-fm-key.file = ../../secrets/di-fm-key.age;
  };

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    virtualHosts."radio.kmein.r" = {
      basicAuthFile = config.age.secrets.ympd-basicAuth.path;
      locations."/" = {
        proxyPass = "http://127.0.0.1:${config.services.ympd.webPort}";
        proxyWebsockets = true;
      };
    };
  };
}
