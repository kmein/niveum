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

  # the DynamicUser's state; /var/lib/readeck is only a symlink to it
  services.restic.backups.niveum.paths = [ "/var/lib/private/readeck" ];
}
