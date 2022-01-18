{ config, pkgs, lib, ... }:
let
  inherit (import <niveum/lib>) retiolumAddresses;
in
{
  imports = [
    ./backup.nix
    ./gaslight.nix
    ./hardware-configuration.nix
    ./kiosk.nix
    ./moodle-dl-meinhark.nix
    ./pulseaudio.nix
    ./tuna.nix
    ./grocy.nix
    ./spotifyd.nix
    <home-manager/nixos>
    <niveum/configs/keyboard.nix>
    <niveum/configs/monitoring.nix>
    <niveum/configs/nix.nix>
    <niveum/configs/printing.nix>
    <niveum/configs/spacetime.nix>
    <niveum/configs/sshd.nix>
    <niveum/configs/traadfri.nix>
    <niveum/configs/tmux.nix>
    <niveum/configs/wpa_supplicant.nix>
    <niveum/modules/retiolum.nix>
  ];

  nix.nixPath = [ "/var/src" ];

  services.logind = {
    lidSwitch = "ignore";
    lidSwitchDocked = "ignore";
    lidSwitchExternalPower = "ignore";
  };

  services.illum.enable = true;

  environment.systemPackages =
  let
    worldradio = pkgs.callPackage <niveum/packages/worldradio.nix> {};
  in [
    (pkgs.writers.writeDashBin "mpv" ''${pkgs.mpv}/bin/mpv --no-video "$@"'')
    (pkgs.writers.writeDashBin "worldradio" ''
      shuf ${worldradio} | ${pkgs.findutils}/bin/xargs ${pkgs.mpv}/bin/mpv --no-video
    '')
    pkgs.git
    pkgs.vim
    pkgs.htop
    pkgs.ncmpcpp
  ];

  networking = {
    hostName = "zaatar";
    wireless.interfaces = [ "wlp2s0" ];
    retiolum = retiolumAddresses.zaatar;
  };

  system.stateVersion = "20.09";
}
