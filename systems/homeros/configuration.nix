{ config, pkgs, ... }:
{
  imports = [
    <niveum/configs/default.nix>
    ./hardware-configuration.nix
  ];

  services.xserver.xrandrHeads = [ { output = "HDMI-1"; primary = true; } "LVDS-1" ];

  environment.systemPackages = [ pkgs.unstable.zeroad pkgs.minecraft ];

  niveum = {
    batteryBlocks.default = "BAT1";
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

  system.stateVersion = "18.09";
}
