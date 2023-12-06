{
  config,
  pkgs,
  lib,
  ...
}: let
  firewall = (import ../../lib).firewall lib;
  inherit (import ../../lib) tmpfilesConfig;

  mukkeMountPoint = "/mnt/mukke";

  streams = import ../../lib/streams.nix {
    di-fm-key = "%DI_FM_KEY%"; # TODO lib.strings.fileContents <secrets/di.fm/key>;
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
    settings = {
      devices = {
        inherit ((import ../../lib).syncthing.devices) kabsa manakish heym;
      };
      folders."${config.services.mpd.musicDirectory}/sync" = {
        devices = ["heym" "kabsa" "manakish"];
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

  fileSystems.${mukkeMountPoint} = {
    device = "//mukke.r/public";
    fsType = "cifs";
    options = [
      "guest"
      "nofail"
      "noauto"
      "ro"
      "rsize=16777216"
      "cache=loose"
    ];
  };

  systemd.tmpfiles.rules = [
    (tmpfilesConfig {
      type = "L+";
      mode = "0644";
      user = "mpd";
      group = "mpd";
      path = "${config.services.mpd.musicDirectory}/mukke";
      argument = mukkeMountPoint;
    })
  ];

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

  systemd.services.mpd-playlists = {
    before = ["mpd.service"];
    wantedBy = ["mpd.service"];
    script = let
      tags = lib.lists.unique (lib.concatMap ({tags ? [], ...}: tags) streams);
      tagStreams = tag: lib.filter ({tags ? [], ...}: lib.elem tag tags) streams;
      makePlaylist = name: streams: pkgs.writeText "${name}.m3u" (lib.concatMapStringsSep "\n" (lib.getAttr "stream") streams);
      playlistDirectory = pkgs.linkFarm "playlists" (
        [
          {
            name = "all.m3u";
            path = makePlaylist "all" streams;
          }
        ]
        ++ map (tag: {
          name = "${tag}.m3u";
          path = makePlaylist tag (tagStreams tag);
        })
        tags
      );
      playlistDirectoryPath = "/var/lib/mpd/playlists";
    in ''
      export DI_FM_KEY="$(cat "$CREDENTIALS_DIRECTORY/di-fm-key")"

      rm -rf ${playlistDirectoryPath}
      mkdir ${playlistDirectoryPath}

      for m3u in $(ls ${playlistDirectory})
      do
        ${pkgs.gnused}/bin/sed s/%DI_FM_KEY%/"$DI_FM_KEY"/g ${playlistDirectory}/"$m3u" > ${playlistDirectoryPath}/"$(basename "$m3u")"
      done
    '';
    serviceConfig = {
      LoadCredential = [
        "di-fm-key:${config.age.secrets.di-fm-key.path}"
      ];
    };
  };

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
