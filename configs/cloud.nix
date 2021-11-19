{ config, lib, pkgs, ... }: {
  imports = [
    <niveum/modules/dropbox.nix>
  ];

  niveum = {
    dropbox.enable = false;
  };

  system.activationScripts.home-symlinks = ''
    ln -sfn ${config.users.users.me.home}/cloud/syncthing/common/mahlzeit ${config.users.users.me.home}/mahlzeit
    ln -sfn ${config.users.users.me.home}/cloud/Seafile/Wiki ${config.users.users.me.home}/notes
    ln -sfn ${config.users.users.me.home}/cloud/Seafile/Uni ${config.users.users.me.home}/uni
  '';

  home-manager.users.me = {
    services.nextcloud-client.enable = true;
  };

  environment.systemPackages = [
    (pkgs.writers.writeDashBin "read" ''
      set -efu
      book="$({
        ${pkgs.findutils}/bin/find ${config.users.users.me.home}/cloud/syncthing/library -type f
        ${pkgs.findutils}/bin/find ${config.users.users.me.home}/cloud/Seafile/Books -type f
      } | ${pkgs.fzf}/bin/fzf)"
      ${pkgs.zathura}/bin/zathura "$book"
    '')
  ];

  fileSystems."/media/moodle" = {
    device = "zaatar.r:/moodle";
    fsType = "nfs";
    options = [
      "x-systemd.idle-timeout=600"
      "noauto"
      "x-systemd.automount"
    ];
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
          "${cloud-dir}/syncthing/common".devices = [ "kabsa" "manakish" ];
          "${cloud-dir}/syncthing/library".devices = [ "kabsa" "manakish" "heym" ];
          "${cloud-dir}/syncthing/mundoiu".devices = [ "kabsa" "manakish" "heym" ];
          "${cloud-dir}/syncthing/music" = {
            devices = [ "kabsa" "manakish" "heym" "zaatar" ];
            id = "music";
          };
        };
    };
  };
}
