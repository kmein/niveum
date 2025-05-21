{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot = {
    initrd = {
      availableKernelModules = [
        "ehci_pci"
        "ahci"
        "usb_storage"
        "sd_mod"
        "sdhci_pci"
      ];
      luks.devices."luks-16a8b3b0-5e0a-4d36-920c-eb6322a3c181".device =
        "/dev/disk/by-uuid/16a8b3b0-5e0a-4d36-920c-eb6322a3c181";
      luks.devices."luks-b3988d35-72a9-4e7c-992d-f500bb388554".device =
        "/dev/disk/by-uuid/b3988d35-72a9-4e7c-992d-f500bb388554";
    };
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/ecab6acb-66f1-41c9-8281-59c76cf8f0c6";
      fsType = "ext4";
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/8066-63C2";
      fsType = "vfat";
    };
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/8cebdb17-4f2e-4750-8e91-71aa942bdfde"; }
  ];

  networking.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
