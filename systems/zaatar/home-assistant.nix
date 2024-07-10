{config, pkgs, lib, ...}: let
  port = 8123;
  inherit (import ../../lib) restic;
  volumeName = "home-assistant";
  streams = import ../../lib/streams.nix {
    di-fm-key = "%DI_FM_KEY%"; # TODO lib.strings.fileContents <secrets/di.fm/key>;
  };
  playlistDirectoryPath = "/var/lib/mpd/playlists";
in {
  networking.firewall.allowedTCPPorts = [port];

  services.nginx.virtualHosts."home.kmein.r" = {
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString port}";
    };
  };

  services.restic.backups.niveum = {
    initialize = true;
    inherit (restic) repository;
    timerConfig = {
      OnCalendar = "daily";
      RandomizedDelaySec = "1h";
    };
    passwordFile = config.age.secrets.restic.path;
    paths = [
      "/var/lib/containers/storage/volumes/${volumeName}"
    ];
  };

  age.secrets = {
    di-fm-key.file = ../../secrets/di-fm-key.age;
  };

  systemd.services.mpd-playlists = {
    before = ["podman-homeassistant.service"];
    wantedBy = ["podman-homeassistant.service"];
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
    in ''
      mkdir -p ${playlistDirectoryPath}

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

  virtualisation.oci-containers = {
    backend = "podman";
    containers.homeassistant = {
      volumes = [
        "${volumeName}:/config"
        "${playlistDirectoryPath}:/media"
      ];
      environment.TZ = "Europe/Berlin";
      image = "ghcr.io/home-assistant/home-assistant:stable";
      extraOptions = [
        "--network=host"
        "--device=/dev/ttyACM0:/dev/ttyACM0" # Example, change this to match your own hardware
      ];
    };
  };
}
