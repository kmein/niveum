{ config, pkgs, lib, ... }:
let
  nixpkgs-unstable = import <nixpkgs-unstable> { config.allowUnfree = true; };
  inherit (import <niveum/lib>) retiolumAddresses;
in
{
  imports = [
    ./hardware-configuration.nix
    <niveum/configs/battery.nix>
    <niveum/configs/default.nix>
    <niveum/configs/networkmanager.nix>
  ];

  niveum = {
    batteryName = "BAT0";
    wirelessInterface = "wlp3s0";
    promptColours.success = "cyan";
  };

  nix = {
    buildCores = 1;
    maxJobs = 2;
  };

  environment.systemPackages = [ nixpkgs-unstable.minecraft ];

  networking = {
    hostName = "kabsa";
    wireless.interfaces = [ "wlp3s0" ];
    retiolum = retiolumAddresses.kabsa;
  };

  system.stateVersion = "19.03";
}
