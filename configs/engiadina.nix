{ config, pkgs, ... }:
let
  inherit (import <niveum/lib>) tmpfilesConfig;
  cdnRoot = "/run/engiadina";
in
{
  systemd.tmpfiles.rules = [
    (tmpfilesConfig {
      type = "d";
      path = cdnRoot;
      mode = "0775";
      user = config.users.users.me.name;
    })
  ];

  services.nginx = {
    enable = true;
    virtualHosts.default = {
      root = cdnRoot;
      listen = [{
        addr = "0.0.0.0";
        port = 3333;
      }];
    };
  };

  environment.shellAliases = {
    engiadina-watch = "${pkgs.findutils}/bin/find extra-src src | ${pkgs.entr}/bin/entr -s 'build-component && ${pkgs.rsync}/bin/rsync -avu dist/*.js ${cdnRoot}/'";
    engiadina-edit = "$EDITOR ${cdnRoot}/index.html";
  };
}
