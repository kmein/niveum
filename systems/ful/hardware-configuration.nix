{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:
{
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
  ];

  boot = {
    loader = {
      systemd-boot = {
        enable = true;
        configurationLimit = 1;
      };
      efi.canTouchEfiVariables = true;
    };
    initrd = {
      availableKernelModules = [
        "virtio_pci"
        "usbhid"
      ];
    };
    kernelModules = [
      "console=ttyS0"
      "console=tty1"
      "nvme.shutdown_timeout=10"
      "libiscsi.debug_libiscsi_eh=1"
    ];
    extraModulePackages = [ ];
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/9bf751c7-cf32-49fb-9354-17694c7a46e0";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/7817-285D";
    fsType = "vfat";
  };

  swapDevices = [ ];
}
