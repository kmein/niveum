{ config, pkgs, lib, ... }:
let
  nixpkgs-unstable = import <nixpkgs-unstable> { config.allowUnfree = true; };
in
{
  imports = [
    <niveum/configs/default.nix>
    <niveum/configs/battery.nix>
    ./hardware-configuration.nix
    # <stockholm/krebs/2configs/hw/x220.nix>
  ];

  niveum = {
    batteryName = "BAT0";
    wirelessInterface = "wlp3s0";
    promptColours.success = "cyan";
  };

  nix.buildCores = 1;
  nix.maxJobs = 2;

  environment.systemPackages = [ nixpkgs-unstable.minecraft ];

  boot.loader.systemd-boot = {
    enable = true;
    configurationLimit = 5;
    consoleMode = "max";
  };

  fileSystems."/mnt/sd-card" = {
    device = "/dev/disk/by-id/mmc-SD32G_0xda0aa352-part1";
    fsType = "vfat";
  };

  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "wilde";

  networking.wireless.interfaces = [ "wlp3s0" ];

  networking.retiolum = {
    ipv4 = "10.243.2.4";
    ipv6 = "42:0:3c46:907c:1fb8:b74f:c59b:1ee3";
  };

  system.stateVersion = "19.03";
}
