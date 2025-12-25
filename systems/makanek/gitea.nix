{ config, pkgs, ... }:
let
  domain = "code.kmein.de";
in {
  services.anubis = {
    defaultOptions.settings = {
      USER_DEFINED_DEFAULT = true;
    };
    instances = let instance = "gitea"; in {
      ${instance}.settings = {
        BIND = "/run/anubis/anubis-${instance}/anubis.sock";
        METRICS_BIND = "/run/anubis/anubis-${instance}/anubis-metrics.sock";
        TARGET = "http://localhost:${toString config.services.gitea.settings.server.HTTP_PORT}";
        USER_DEFINED_INSTANCE = true;
        OG_PASSTHROUGH = true;
        SERVE_ROBOTS_TXT = true;
      };
    };
  };

  users.users.nginx.extraGroups = [ config.services.anubis.instances."gitea".group ];

  services.gitea = {
    enable = true;
    appName = domain;
    settings = {
      server.ROOT_URL = "https://${domain}";
      server.DOMAIN = domain;
      server.SSH_PORT = pkgs.lib.niveum.sshPort;
      service.DISABLE_REGISTRATION = true;
    };
  };
  services.nginx.virtualHosts.${domain} = {
    forceSSL = true;
    enableACME = true;
    # locations."/".extraConfig = "proxy_pass http://localhost:3000;";
    locations = {
      "/" = {
        proxyPass = "http://unix:${config.services.anubis.instances."gitea".settings.BIND}";
        proxyWebsockets = true;
      };
      "/metrics".proxyPass = "http://unix:${config.services.anubis.instances."gitea".settings.METRICS_BIND}";
    };
  };

  niveum.passport.services = [
    {
      link = domain;
      title = "Gitea";
      description = "hosts a couple of <tt>git</tt> repos. Registration is disabled.";
    }
  ];
}
