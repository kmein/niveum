let
  port = 9090;
in
{
  virtualisation.oci-containers = {
    backend = "podman";
    containers.xanado = {
      volumes = [ ];
      ports = [ "${toString port}:9093" ];
      environment.TZ = "Europe/Berlin";
      image = "ghcr.io/cdot/xanado:v3.1.6";
    };
  };

  services.nginx.virtualHosts."scrabble.kmein.de" = {
    enableACME = true;
    forceSSL = true;
    locations."/".proxyPass = "http://localhost:${toString port}";
  };
}
