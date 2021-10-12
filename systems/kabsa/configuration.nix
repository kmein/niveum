{ config, pkgs, lib, ... }:
let
  nixpkgs-unstable = import <nixpkgs-unstable> { config.allowUnfree = true; };
  inherit (import <niveum/lib>) retiolumAddresses;
in
{
  imports = [
    <niveum/configs/default.nix>
    <niveum/configs/battery.nix>
    <niveum/configs/networkmanager.nix>
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

  networking.hostName = "kabsa";

  networking.wireless.interfaces = [ "wlp3s0" ];

  networking.retiolum = retiolumAddresses.kabsa;

  system.stateVersion = "19.03";
}
