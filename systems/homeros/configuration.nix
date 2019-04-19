{ config, pkgs, ... }:
{
  imports = [
    <configs>
    {
      services.xserver.xrandrHeads = [ "LVDS1" { output = "HDMI1"; primary = true; } ];
    }
    {
      environment.systemPackages =
      let unstable = import <nixos-unstable> {};
      in [ unstable.zeroad ];
    }
    ./hardware-configuration.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "homeros";

  system.stateVersion = "18.09";
}
