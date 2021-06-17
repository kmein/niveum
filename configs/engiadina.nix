{ config, pkgs, ... }:
let
  inherit (import <niveum/lib>) tmpfilesConfig;
  cdnRoot = "/var/lib/engiadina";
in
{
  imports = [ <stockholm/krebs/3modules/permown.nix> ];

  krebs.permown.${cdnRoot} = {
    owner = config.users.users.me.name;
    group = "users";
    umask = "0002";
  };

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
