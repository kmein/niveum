{
  pkgs,
  lib,
  config,
  ...
}:
{
  programs.hyprland = {
    enable = true;
    withUWSM = true;
    xwayland.enable = true;
    package = pkgs.hyprland;
    portalPackage = pkgs.xdg-desktop-portal-hyprland;
  };

  programs.ydotool.enable = true;

  # hyprlock has no PAM service by default (home-manager can't create one), so
  # it falls back to pam_deny and cannot authenticate at all. Create it here.
  # Deliberately without fprintd: hyprlock does fingerprint via its own fprintd
  # D-Bus backend that runs in parallel with the password prompt, so keeping
  # pam_fprintd out of PAM keeps password unlock instant instead of blocking on
  # a swipe first (which is exactly what makes swaylock feel slow).
  security.pam.services.hyprlock.fprintAuth = false;

  xdg.portal = {
    enable = true;
    extraPortals = [
      pkgs.xdg-desktop-portal-hyprland
      pkgs.xdg-desktop-portal-gtk
    ];
    config.common.default = "*";
  };

  services.dbus = {
    implementation = "broker";
    # needed for GNOME services outside of GNOME (?)
    packages = [ pkgs.gcr ];
  };

  environment.systemPackages = [
    pkgs.xdg-desktop-portal
    pkgs.xdg-desktop-portal-hyprland
  ];

  services.getty.autologinOnce = true;
  services.getty.autologinUser = config.users.users.me.name;

  home-manager.users.me = import ./home-manager.nix {
    inherit lib pkgs config;
  };
}
