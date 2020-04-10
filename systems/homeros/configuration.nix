{ config, pkgs, lib, ... }:
{
  imports = [
    <niveum/configs/default.nix>
    ./hardware-configuration.nix
  ];

  services.xserver.xrandrHeads = [ { output = "HDMI1"; primary = true; } "LVDS1" ];

  environment.systemPackages = [ pkgs.unstable.zeroad pkgs.minecraft ];

  niveum = {
    batteryBlocks.default = "BAT1";
    networkInterfaces.wireless = "wlp3s0";
    promptColours.success = "green";
    fonts.size = lib.mkForce 11;
  };

  boot.loader.systemd-boot = {
    enable = true;
    configurationLimit = 5;
  };

  boot.loader.efi.canTouchEfiVariables = true;

  # ref https://askubuntu.com/questions/575020/ubuntu-14-04-brightness-problem-lenovo-z500
  boot.kernelParams = [ "acpi_backlight=vendor" ];
  environment.etc."X11/xorg.conf.d/80-backlight.conf".text = ''
    Section "Device"
        Identifier  "Intel Graphics"
        Driver      "intel"
        Option      "AccelMethod"     "sna"
        Option      "Backlight"       "ideapad"
        BusID       "PCI:0:2:0"
    EndSection
  '';

  services.xserver.videoDrivers = [ "intel" ];

  networking.retiolum = {
    ipv4 = "10.243.2.1";
    ipv6 = "42:0:3c46:53e:e63d:e62a:56ea:c705";
  };

  networking.hostName = "homeros";

  system.stateVersion = "18.09";
}
