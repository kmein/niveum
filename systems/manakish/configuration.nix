{ config, pkgs, ... }:
let
  inherit (import <niveum/lib>) retiolumAddresses;
in
{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    <niveum/configs/default.nix>
    <niveum/configs/battery.nix>
    <niveum/configs/wpa_supplicant.nix>
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
  networking.wireless.interfaces = [ "wlp3s0" ];

  environment.systemPackages = with pkgs; [
    git vim
  ];

  networking.retiolum = retiolumAddresses.manakish;

  networking.hostName = "manakish";

  system.stateVersion = "20.09"; # Did you read the comment?
}
