{ config, lib, pkgs, ... }: {
  imports = [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix> ];

  boot = {
    initrd = {
      availableKernelModules = [ "ehci_pci" "ahci" "xhci_pci" "usb_storage" "sd_mod" "sdhci_pci" ];
      luks.devices."luksmap".device = "/dev/disk/by-uuid/03b6abd0-e9ce-49c8-9659-a1d94f645d0f";
      kernelModules = [ ];
    };
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot = {
        enable = true;
        configurationLimit = 5;
        consoleMode = "max";
      };
    };
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/66ced7b9-cfa7-40dd-a488-18ef91a337e0";
      fsType = "ext4";
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/E1B1-1B9C";
      fsType = "vfat";
    };
    "/mnt/sd-card" = {
      device = "/dev/disk/by-id/mmc-SD32G_0xda0aa352-part1";
      fsType = "vfat";
    };
  };

  zramSwap.enable = false;

  nix.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
