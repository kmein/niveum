{ config, pkgs, ... }:
{
  imports = [
    <configs/default.nix>
    ./hardware-configuration.nix
  ];

  services.xserver.xrandrHeads = [ "LVDS1" { output = "HDMI1"; primary = true; } ];

  environment.systemPackages = [ pkgs.unstable.zeroad pkgs.minecraft ];

  niveum = {
    networkInterfaces.wireless = "wlp3s0";
    promptColours.success = "green";
  };

  boot.loader.systemd-boot = {
    enable = true;
    configurationLimit = 5;
  };

  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelParams = [
    "acpi_backlight=none"
    "acpi_backlight=video"
    "acpi_backlight=vendor"
    "acpi_backlight=native"
  ];

  networking.hostName = "homeros";

  system.stateVersion = "19.03";
}
