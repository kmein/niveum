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
    services.nextcloud-client.enable = false;
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
        let cloud-dir = "${config.users.users.me.home}/cloud";
        in {
          "${cloud-dir}/syncthing/common".devices = [ "wilde" "manakish" ];
          "${cloud-dir}/syncthing/library".devices = [ "wilde" "manakish" "heym" ];
          "${cloud-dir}/syncthing/mundoiu".devices = [ "wilde" "manakish" "heym" ];
          "${cloud-dir}/syncthing/music".devices = [ "wilde" "manakish" "heym" ];
          "${cloud-dir}/moodle" = {
            devices = [ "wilde" "toum" "manakish" ];
            id = "moodle-dl";
          };
        };
    };
  };
}
