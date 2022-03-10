{
  networking.firewall.allowedTCPPorts = [8123];

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
