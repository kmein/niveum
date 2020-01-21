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

  hardware.bumblebee = {
    enable = true;
    connectDisplay = true;
  };
  hardware.opengl.driSupport32Bit = true;
  services.xserver.videoDrivers = [ "intel" "nvidia" ];

  networking.hostName = "homeros";

  system.stateVersion = "18.09";
}
