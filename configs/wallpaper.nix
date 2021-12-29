{ pkgs, lib, ... }:
{
  krebs.fetchWallpaper = {
    enable = true;
    url = "http://prism.r/realwallpaper-krebs-stars-berlin.png";
  };

  users.users.fetchWallpaper.isSystemUser = true;

  services.xserver = {
    display = lib.mkForce 0; # needed for fetchWallpaper to find the X display
    displayManager.sessionCommands = "${pkgs.xorg.xhost}/bin/xhost +LOCAL:";
  };
}
