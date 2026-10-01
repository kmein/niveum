{
  pkgs,
  lib,
  config,
  ...
}:
{
  # the compositor itself (niri), portals, ydotool and the desktop tool set come
  # from niphas.nixosModules.desktop; what is left here is the session plumbing
  # around it.

  services.dbus = {
    implementation = "broker";
    # needed for GNOME services outside of GNOME (?)
    packages = [ pkgs.gcr ];
  };

  services.getty.autologinOnce = true;
  services.getty.autologinUser = config.users.users.me.name;

  home-manager.users.me = import ./home-manager.nix {
    inherit lib pkgs config;
  };
}
