{ config, pkgs, ... }:
{
  services.sgraebbel = {
    enable = true;
    port = 9090;
  };

  services.nginx.virtualHosts."scrabble.${pkgs.lib.niveum.domain}" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://localhost:${toString config.services.sgraebbel.port}";
      # moves reach the other players over server-sent events
      extraConfig = "proxy_buffering off;";
    };
  };

  services.restic.backups.niveum.paths = [
    "/var/lib/private/sgraebbel"
    # games of the former xanado server
    "/var/lib/xanado"
  ];
}
