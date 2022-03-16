{
  lib,
  config,
  pkgs,
  ...
}: let
  inherit (import <niveum/lib>) retiolumAddresses;
in {
  imports = [
    ./hardware-configuration.nix
    <niveum/configs/monitoring.nix>
    <niveum/configs/nix.nix>
    <niveum/configs/save-space.nix>
    <niveum/configs/spacetime.nix>
    <niveum/configs/sshd.nix>
    <niveum/modules/retiolum.nix>
  ];

  nix.nixPath = ["/var/src"];

  networking = {
    hostName = "tahina";
    retiolum = retiolumAddresses.tahina;
  };

  system.stateVersion = "22.05";

  environment.systemPackages = [pkgs.vim pkgs.git pkgs.tmux pkgs.python3];
}
