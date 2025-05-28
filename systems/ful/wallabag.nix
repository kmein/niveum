{ pkgs, config, ... }:

let
  domain = "pocket.kmein.de";
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

  virtualisation.oci-containers.containers."${domain}" = {
    autoStart = true;
    image = "wallabag/wallabag:2.6.12";
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
