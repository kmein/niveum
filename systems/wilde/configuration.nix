{ config, pkgs, lib, ... }:

{
  imports = [
    <configs/default.nix>
    ./hardware-configuration.nix
    <stockholm/krebs/2configs/hw/x220.nix>
  ];

  virtualisation.docker.enable = lib.mkForce false;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "wilde";

  system.stateVersion = "19.03";
}
