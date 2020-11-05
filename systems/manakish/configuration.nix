# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    <niveum/configs/default.nix>
    {
      boot.extraModulePackages = with config.boot.kernelPackages; [
        tp_smapi
        acpi_call
      ];
      boot.kernelModules = [ "tp_smapi" "acpi_call" ];
      environment.systemPackages = [ pkgs.tpacpi-bat ];

      services.tlp = {
        enable = true;
        settings = {
          START_CHARGE_THRESH_BAT0 = 80;
          STOP_CHARGE_THRESH_BAT0 = 95;
        };
      };
    }
  ];


  niveum = {
    batteryName = "BAT0";
    wirelessInterface = "wlp3s0";
    promptColours.success = "cyan";
  };

  networking.useDHCP = false;
  networking.interfaces = {
    enp0s25.useDHCP = true;
    wlp3s0.useDHCP = true;
    wwp0s20u4i6.useDHCP = true;
  };

  environment.systemPackages = with pkgs; [
    git vim
  ];

  networking.retiolum = {
    ipv4 = "10.243.2.85";
    ipv6 = "42:0:3c46:ac99:ae36:cb8:c551:ba27";
  };

  networking.hostName = "manakish";

  system.stateVersion = "20.09"; # Did you read the comment?
}
