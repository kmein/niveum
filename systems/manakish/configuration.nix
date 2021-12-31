{ config, pkgs, ... }:
let
  inherit (import <niveum/lib>) retiolumAddresses;
in
{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./hdmi.nix
    <niveum/configs/default.nix>
    <niveum/configs/battery.nix>
    <niveum/configs/wpa_supplicant.nix>
  ];

  niveum = {
    batteryName = "BAT0";
    wirelessInterface = "wlp3s0";
    promptColours.success = "green";
  };

  networking = {
    useDHCP = false;
    interfaces = {
      enp0s25.useDHCP = true;
      wlp3s0.useDHCP = true;
      wwp0s20u4i6.useDHCP = true;
    };
    wireless.interfaces = [ "wlp3s0" ];
    retiolum = retiolumAddresses.manakish;
    hostName = "manakish";
  };

  system.stateVersion = "20.09"; # Did you read the comment?
}
