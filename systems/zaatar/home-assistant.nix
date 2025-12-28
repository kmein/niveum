{
  config,
  pkgs,
  lib,
  ...
}:
let
  port = 8123;
  volumeName = "home-assistant";
in
{
  networking.firewall.allowedTCPPorts = [ port ];

  services.nginx.virtualHosts."home.kmein.r" = {
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString port}";
    };
  };

  services.restic.backups.niveum = {
    initialize = true;
    repository = pkgs.lib.niveum.restic.repository;
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

  hardware.bluetooth.enable = true;

  systemd.services.update-containers = {
    startAt = "Mon 02:00";
    script = ''
      images=$(${pkgs.podman}/bin/podman ps -a --format="{{.Image}}" | sort -u)

      for image in $images; do
        ${pkgs.podman}/bin/podman pull "$image"
      done
    '';
    serviceConfig = {
      Type = "oneshot";
      Restart = "on-failure";
      RestartSec = "1h";
    };
  };

  systemd.services.restart-homeassistant = {
    startAt = "Tue 02:00";
    script = ''
      ${pkgs.systemd}/bin/systemctl try-restart podman-homeassistant.service
    '';
    serviceConfig = {
      Type = "oneshot";
    };
  };

  virtualisation.podman = {
    enable = true;
    autoPrune = {
      enable = true;
      flags = [ "--all" ];
    };
  };

  virtualisation.oci-containers = {
    backend = "podman";
    containers.homeassistant = {
      volumes = [
        "${volumeName}:/config"
        "/run/dbus:/run/dbus:ro"
      ];
      # needed for bluetooth
      capabilities.NET_ADMIN = true;
      capabilities.NET_RAW = true;
      environment.TZ = "Europe/Berlin";
      image = "ghcr.io/home-assistant/home-assistant:stable";
      extraOptions = [
        "--network=host"
        "--device=/dev/ttyACM0:/dev/ttyACM0" # Example, change this to match your own hardware
      ];
    };
  };
}
