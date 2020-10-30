{ config, pkgs, lib, ... }: {
  imports = [
    <niveum/configs/default.nix>
    <niveum/configs/kdeconnect.nix>
    ./hardware-configuration.nix
  ];

  environment.systemPackages = [ pkgs.zeroad pkgs.minecraft ];

  niveum = {
    batteryName = "BAT1";
    wirelessInterface = "wlp3s0";
    promptColours.success = "green";
  };

  boot.loader.systemd-boot = {
    enable = true;
    configurationLimit = 5;
  };

  boot.loader.efi.canTouchEfiVariables = true;

  # ref https://askubuntu.com/questions/575020/ubuntu-14-04-brightness-problem-lenovo-z500
  boot.kernelParams = [ "acpi_backlight=vendor" ];
  services.xserver.deviceSection = ''
    Option "Backlight" "ideapad"
    BusID "PCI:0:2:0"
  '';

  services.xserver.videoDrivers = [ "intel" ];

  networking.retiolum = {
    ipv4 = "10.243.2.1";
    ipv6 = "42:0:3c46:53e:e63d:e62a:56ea:c705";
  };

  networking.hostName = "homeros";

  system.stateVersion = "18.09";
}
