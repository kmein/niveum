{ config, ... }:
{
  networking.firewall.allowedTCPPorts = [ 1965 ];
  services.agate = {
    enable = true;
    addresses = [ "0.0.0.0:1965" ];
    hostnames = [ "kmein.de" ];
    language = "de";
  };

  services.restic.backups.niveum.paths = [
    config.services.agate.contentDir
    config.services.agate.certificatesDir
  ];
}
