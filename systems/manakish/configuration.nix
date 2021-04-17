# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    <niveum/configs/default.nix>
    <niveum/configs/battery.nix>
    {
      programs.steam.enable = true;
      nixpkgs.config.steam.java = true;
    }
    {
      services.xserver.displayManager.sessionCommands =
      let
        intern = "LVDS-1";
        extern = "HDMI-1";
        pulseaudioCard = "alsa_card.pci-0000_00_1b.0";
        pulseaudioProfile = "output:hdmi-stereo+input:analog-stereo";
      in toString (pkgs.writers.writeDash "hdmi-on" ''
        ${pkgs.xorg.xrandr}/bin/xrandr --output ${intern} --primary --auto --output ${extern} --above ${intern} --auto
        ${pkgs.pulseaudio}/bin/pactl set-card-profile ${pulseaudioCard} ${pulseaudioProfile}
      '');
    }
  ];

  niveum = {
    batteryName = "BAT0";
    wirelessInterface = "wlp3s0";
    promptColours.success = "green";
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
