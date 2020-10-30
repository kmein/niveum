{ config, pkgs, lib, ... }:
let
  inherit (import <niveum/lib>) nixpkgs-unstable;
  unstable = import nixpkgs-unstable {
    config.allowUnfree = true; 
  };
in
{
  imports = [
    <niveum/configs/default.nix>
    ./hardware-configuration.nix
    <stockholm/krebs/2configs/hw/x220.nix>
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
    batteryBlocks.default = "BAT0";
    networkInterfaces.wireless = "wlp3s0";
    promptColours.success = "cyan";
  };

  virtualisation.docker.enable = lib.mkForce false;

  nix.buildCores = 2;

  environment.systemPackages = [ unstable.minecraft ];

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

  networking.retiolum = {
    ipv4 = "10.243.2.4";
    ipv6 = "42:0:3c46:907c:1fb8:b74f:c59b:1ee3";
  };

  system.stateVersion = "19.03";
}
