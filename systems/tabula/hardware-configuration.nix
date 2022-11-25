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
    loader.grub = {
      enable = true;
      version = 2;
      device = "/dev/sda";
    };
    initrd = {
      availableKernelModules = ["pata_sis" "ohci_pci" "ehci_pci" "sata_sis" "usb_storage" "sd_mod" "sr_mod"];
      kernelModules = [];
    };
    kernelModules = [];
    extraModulePackages = [];
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/6fe3c127-068f-4b71-9cac-5fea66f66dc8";
    fsType = "ext4";
  };

  swapDevices = [
    {device = "/dev/disk/by-uuid/874256aa-5bae-44a4-8933-c65f8600fe78";}
  ];

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
