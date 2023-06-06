{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}: {
  imports = [(modulesPath + "/profiles/qemu-guest.nix")];

  boot = {
    initrd = {
      availableKernelModules = ["ata_piix" "virtio_pci" "xhci_pci" "sd_mod" "sr_mod"];
      kernelModules = [];
    };
    kernelModules = [];
    extraModulePackages = [];
    loader.grub = {
      enable = true;
      devices = ["/dev/sda"];
      configurationLimit = 3;
    };
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/9eaeaaa7-b453-4634-8a69-d416f702d3aa";
    fsType = "ext4";
  };

  swapDevices = [];
  zramSwap.enable = true;

  nix.settings.max-jobs = lib.mkDefault 2;
}
