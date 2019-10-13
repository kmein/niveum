{ config, pkgs, lib, ... }:

{
  imports = [
    <configs/default.nix>
    ./hardware-configuration.nix
    <stockholm/krebs/2configs/hw/x220.nix>
  ];

  niveum = {
    batteryBlocks.default = "BAT0";
    networkInterfaces.wireless = "wlp3s0";
    promptColours.success = "cyan";
  };

  boot.extraModulePackages = with config.boot.kernelPackages; [ tp_smapi acpi_call ];

  boot.kernelModules = [ "tp_smapi" "acpi_call" ];

  environment.systemPackages = [ pkgs.tpacpi-bat pkgs.minecraft ];

  virtualisation.docker.enable = lib.mkForce false;

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

  networking.hostName = "wilde";

  system.stateVersion = "19.09";
}
