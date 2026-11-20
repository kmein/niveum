{ config, pkgs, ... }:
let
  domain = "read.${pkgs.lib.niveum.domain}";
  port = 8000;
in
{
  # readeck derives its token, session and TOTP keys from the secret key,
  # and the module rewrites config.toml on every start
  age.secrets.readeck.file = ../../secrets/readeck.age;

  services.readeck = {
    enable = true;
    environmentFile = config.age.secrets.readeck.path;
    settings.server = {
      host = "127.0.0.1";
      inherit port;
    };
  };

  services.nginx.virtualHosts.${domain} = {
    enableACME = true;
    forceSSL = true;
    locations."/".proxyPass = "http://127.0.0.1:${toString port}";
  };

  services.nginx.virtualHosts."pocket.${pkgs.lib.niveum.domain}" = {
    enableACME = true;
    forceSSL = true;
    locations."/".return = "301 https://${domain}/";
  };

  services.restic.backups.niveum.paths = [
    # the DynamicUser's state; /var/lib/readeck is only a symlink to it
    "/var/lib/private/readeck"
    # wallabag's data, kept after its retirement until deleted by hand
    "/var/lib/wallabag"
  ];
}
