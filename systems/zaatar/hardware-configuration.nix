{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:
{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot = {
    initrd.availableKernelModules = [
      "ahci"
      "xhci_pci"
      "usb_storage"
      "sd_mod"
      "sdhci_acpi"
      "rtsx_usb_sdmmc"
    ];
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
    supportedFilesystems = [ "ntfs" ];
    loader = {
      systemd-boot = {
        enable = true;
        configurationLimit = 5;
      };
      efi.canTouchEfiVariables = true;
    };
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/5dcaa7fe-08e7-46a6-ac93-b267eeb144eb";
      fsType = "ext4";
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/CE3B-F4C6";
      fsType = "vfat";
    };
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/7b2a3e4c-e53f-4c53-b599-b6d6cff49f1f"; }
  ];

  nix.settings.max-jobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
