{ config, pkgs, ... }:
{
  imports = [
    <configs>
    {
      services.xserver.xrandrHeads = [ "eDP1" ];
    }
    ./hardware-configuration.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "scardanelli";

  system.stateVersion = "18.09";
}
