let
  port = 8123;
in {
  networking.firewall.allowedTCPPorts = [port];

  services.nginx.virtualHosts."home.kmein.r" = {
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString port}";
    };
  };

  virtualisation.oci-containers = {
    backend = "podman";
    containers.homeassistant = {
      volumes = ["home-assistant:/config"];
      environment.TZ = "Europe/Berlin";
      image = "ghcr.io/home-assistant/home-assistant:stable";
      extraOptions = [
        "--network=host"
        # "--device=/dev/ttyUSB0:/dev/ttyACM0"  # Example, change this to match your own hardware
      ];
    };
  };
}
