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

  services.restic.backups.niveum.paths = [
    "/var/lib/containers/storage/volumes/${volumeName}"
  ];

  age.secrets = {
    di-fm-key.file = ../../secrets/di-fm-key.age;
  };

  hardware.bluetooth.enable = true;

  systemd.services.restart-homeassistant = {
    startAt = "Tue 02:00";
    script = ''
      ${pkgs.systemd}/bin/systemctl try-restart podman-homeassistant.service
    '';
    serviceConfig = {
      Type = "oneshot";
    };
  };

  virtualisation.oci-containers = {
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
        "--device=/dev/ttyACM0:/dev/ttyACM0"
      ];
    };
  };
}
