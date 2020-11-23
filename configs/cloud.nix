{ config, lib, pkgs, ... }: {
  imports = [
    <niveum/modules/seafile.nix>
    <niveum/modules/dropbox.nix>
  ];

  niveum = {
    dropbox.enable = true;
    seafile.enable = true;
  };

  system.activationScripts.home-symlinks = ''
    ln -sfn ${config.users.users.me.home}/cloud/syncthing/common/mahlzeit ${config.users.users.me.home}/mahlzeit
    ln -sfn ${config.users.users.me.home}/cloud/Seafile/Wiki ${config.users.users.me.home}/notes
    ln -sfn ${config.users.users.me.home}/cloud/Seafile/Uni ${config.users.users.me.home}/uni
  '';

  home-manager.users.me = {
    services.nextcloud-client.enable = true;
  };

  services.syncthing = rec {
    enable = true;
    user = "kfm";
    openDefaultPorts = true;
    configDir = "/home/kfm/.config/syncthing";
    dataDir = "/home/kfm/.config/syncthing";
    declarative = rec {
      cert = toString <system-secrets/syncthing/cert.pem>;
      key = toString <system-secrets/syncthing/key.pem>;
      inherit ((import <niveum/lib>).syncthing) devices;
      folders =
        let syncthing-dir = "${config.users.users.me.home}/cloud/syncthing";
        in {
          "${syncthing-dir}/common".devices = [ "wilde" "manakish" ];
          "${syncthing-dir}/library".devices = lib.attrNames devices;
          "${syncthing-dir}/mundoiu".devices = lib.attrNames devices;
          "${syncthing-dir}/music".devices = lib.attrNames devices;
        };
    };
  };
}
