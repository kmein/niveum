{ pkgs, lib, ... }:
{
  hardware.bluetooth = {
    enable = true;
    extraConfig = lib.generators.toINI {} {
      General.Enable = lib.concatStringsSep "," ["Source" "Sink" "Media" "Socket"];
    };
  };

  environment.systemPackages = [ pkgs.blueman ];

  home-manager.users.me = {
    services.blueman-applet.enable = false;
  };
}
