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

  # hyprlock has no PAM service by default (home-manager can't create one), so
  # it falls back to pam_deny and cannot authenticate at all. Create it here.
  security.pam.services.hyprlock = { };

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
