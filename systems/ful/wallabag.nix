{ pkgs, config, ... }:

let
  domain = "pocket.${pkgs.lib.niveum.domain}";
  port = "8088";
  dataPath = "/var/lib/wallabag";
in
{
  services.nginx.virtualHosts.${domain} = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${port}";
      proxyWebsockets = true;
    };
    extraConfig = ''
      client_body_timeout 3000s;
      client_header_timeout 3000s;
      keepalive_timeout 3000s;
      proxy_read_timeout 3000s;
      proxy_connect_timeout 3000s;
      proxy_send_timeout 3000s;
    '';
  };

  services.restic.backups.niveum.paths = [ dataPath ];

  systemd.services.restart-wallabag = {
    startAt = "Tue 02:00";
    script = ''
      ${pkgs.systemd}/bin/systemctl try-restart podman-${domain}.service
    '';
    serviceConfig = {
      Type = "oneshot";
    };
  };

  virtualisation.oci-containers.containers."${domain}" = {
    autoStart = true;
    image = "wallabag/wallabag:latest";
    ports = [ "${port}:80" ];
    volumes = [
      "${dataPath}/data:/var/www/wallabag/data"
      "${dataPath}/images:/var/www/wallabag/web/assets/images"
    ];
    environment = {
      SYMFONY__ENV__DOMAIN_NAME = "https://${domain}";
      SYMFONY__ENV__FOSUSER_CONFIRMATION = "false";
      PHP_MEMORY_LIMIT = "512M";
      SYMFONY__ENV__SERVER_NAME = "Wallabag";
    };
    extraOptions = [ "--pull=always" ];
  };
}
