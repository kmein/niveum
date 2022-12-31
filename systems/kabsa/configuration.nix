{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (import <niveum/lib>) retiolumAddresses;
in {
  imports = [
    ./hardware-configuration.nix
    <niveum/configs/battery.nix>
    <niveum/configs/default.nix>
    <niveum/configs/networkmanager.nix>
    ./mastodon.nix
  ];

  niveum = {
    batteryName = "BAT0";
    wirelessInterface = "wlp3s0";
    promptColours.success = "cyan";
  };

  nix.settings = {
    cores = 1;
    max-jobs = 2;
  };

  environment.systemPackages = [pkgs.minecraft pkgs.zeroad];

  networking = {
    hostName = "kabsa";
    wireless.interfaces = ["wlp3s0"];
    retiolum = retiolumAddresses.kabsa;
  };

  system.stateVersion = "19.03";
}
