{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}: {
  imports = [(modulesPath + "/installer/scan/not-detected.nix")];

  boot = {
    initrd = {
      availableKernelModules = ["xhci_pci" "ehci_pci" "ahci" "usb_storage" "sd_mod" "sdhci_pci"];
      kernelModules = [];
      luks.devices."crypted".device = "/dev/disk/by-uuid/493cb228-c292-4f71-9f38-dcb3e96dec47";
    };
    kernelModules = ["kvm-intel"];
    extraModulePackages = [];
    loader.grub = {
      enable = true;
      version = 2;
      efiSupport = true;
      efiInstallAsRemovable = true;
      device = "/dev/sda";
      configurationLimit = 5;
    };
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/ff47c479-e6d4-4cd6-93f1-9685f4018bc6";
      fsType = "ext4";
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/D4AC-91B0";
      fsType = "vfat";
    };
  };

  swapDevices = [];
  zramSwap.enable = true;

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
