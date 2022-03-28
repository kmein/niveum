{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot = {
    loader = {
      systemd-boot = {
        enable = true;
        consoleMode = "max";
      };
      efi.canTouchEfiVariables = true;
    };
    initrd = {
      availableKernelModules = ["xhci_pci" "ehci_pci" "ahci" "firewire_ohci" "usb_storage" "sd_mod" "sr_mod" "sdhci_pci"];
      kernelModules = ["dm-snapshot"];
      luks.devices.luksmap.device = "/dev/disk/by-uuid/b7d66981-8cb7-4aad-a595-ee6574b312cf";
    };
    kernelModules = ["kvm-intel"];
    extraModulePackages = [];
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/e9a8bd34-61eb-4317-888d-bd7d6248a906";
    fsType = "xfs";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/9B2F-31E1";
    fsType = "vfat";
  };

  swapDevices = [];

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
