{
  pkgs,
  lib,
  config,
  ...
}:
let
  stylixColors = config.lib.stylix.colors;
in
{
  programs.hyprland = {
    enable = true;
    withUWSM = true;
    xwayland.enable = true;
    package = pkgs.hyprland;
    portalPackage = pkgs.xdg-desktop-portal-hyprland;
  };

  programs.ydotool.enable = true;

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

  services.displayManager.autoLogin = {
    enable = true;
    user = config.users.users.me.name;
  };

  home-manager.users.me = import ./home-manager.nix {
    inherit lib pkgs config;
  };
}
