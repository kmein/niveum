{ config, pkgs, ... }:
{
  imports = [
    <modules/seafile.nix>
    <modules/google-drive.nix>
    <modules/dropbox.nix>
  ];

  niveum.dropbox.enable = true;

  niveum.seafile.enable = true;

  niveum.google-drive = {
    enable = true;
    directory = "${config.users.users.me.home}/cloud/gdrive";
    user = config.users.users.me;
  };

  services.syncthing = rec {
    enable = true;
    # user = config.users.users.me.name;
    # dataDir = "${config.users.users.me.home}/.config/syncthing";
    user = "kfm";
    dataDir = "/home/${user}/.config/syncthing";
    openDefaultPorts = true;
  };

  home-manager.users.me = {
    services.syncthing = {
      enable = true;
      tray = true;
    };
  };
}
