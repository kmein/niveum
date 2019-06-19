{ config, pkgs, ... }:
{
  imports = [
    <configs/default.nix>
    ./hardware-configuration.nix
  ];

  services.xserver.xrandrHeads = [ "eDP1" ];

  niveum = {
    networkInterfaces.wireless = "wlp2s0";
    promptColours.success = "yellow";
  };

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "scardanelli";

  system.stateVersion = "19.03";
}
