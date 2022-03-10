{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [<nixpkgs/nixos/modules/profiles/qemu-guest.nix>];

  boot = {
    initrd = {
      availableKernelModules = ["ata_piix" "virtio_pci" "xhci_pci" "sd_mod" "sr_mod"];
      kernelModules = [];
    };
    kernelModules = [];
    extraModulePackages = [];
    loader.grub = {
      enable = true;
      version = 2;
      devices = ["/dev/sda"];
    };
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/9eaeaaa7-b453-4634-8a69-d416f702d3aa";
    fsType = "ext4";
  };

  swapDevices = [];
  zramSwap.enable = true;

  nix.maxJobs = lib.mkDefault 2;
}
