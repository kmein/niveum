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

  systemd.services.restart-wallabag = {
    startAt = "Tue 02:00";
    script = ''
      ${pkgs.systemd}/bin/systemctl try-restart podman-${domain}.service
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

  virtualisation.oci-containers.backend = "podman";
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
