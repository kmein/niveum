{config, ...}: let
  port = 8123;
  inherit (import ../../lib) restic;
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
      "/var/lib/containers/storage/volumes/home-assistant.bak/_data/backups"
    ];
  };

  virtualisation.oci-containers = {
    backend = "podman";
    containers.homeassistant = {
      volumes = ["home-assistant.bak:/config"];
      environment.TZ = "Europe/Berlin";
      image = "ghcr.io/home-assistant/home-assistant:stable";
      extraOptions = [
        "--network=host"
        "--device=/dev/ttyACM0:/dev/ttyACM0" # Example, change this to match your own hardware
      ];
    };
  };
}
