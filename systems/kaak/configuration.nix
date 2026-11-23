{
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ../../configs/desktop-core.nix
    ../../configs/networkmanager.nix
  ];

  # whatever machine the stick is plugged into
  hardware.enableAllHardware = true;
  hardware.enableRedistributableFirmware = true;
  hardware.cpu.intel.updateMicrocode = true;
  hardware.cpu.amd.updateMicrocode = true;
  nixpkgs.hostPlatform = "x86_64-linux";

  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 3;
  # never write the nvram of a borrowed machine; bootctl still installs
  # EFI/BOOT/BOOTX64.EFI, the removable-media fallback
  boot.loader.efi.canTouchEfiVariables = false;

  # partitions as kaak-install creates them
  boot.initrd.luks.devices.kaak = {
    device = "/dev/disk/by-partlabel/kaak-crypt";
    allowDiscards = true;
  };
  fileSystems."/" = {
    device = "/dev/mapper/kaak";
    fsType = "ext4";
    options = [ "noatime" ];
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-partlabel/kaak-boot";
    fsType = "vfat";
    options = [ "umask=0077" ];
  };
  # the vm has no stick to unlock
  virtualisation.vmVariant.boot.initrd.luks.devices = lib.mkForce { };

  networking.hostName = "kaak";
  system.stateVersion = "26.05";

  # the luks passphrase is the only gate
  users.users.me.hashedPassword = "";
  security.sudo.wheelNeedsPassword = false;

  # swaylock cannot unlock an empty password; Mod+Shift+W only drops
  # passage's identity
  niphas.locker.lockBeforeSleep = false;
  niphas.locker.package = lib.mkForce (
    pkgs.writers.writeDashBin "lock-screen" ''
      exec ${lib.getExe pkgs.passage} lock
    ''
  );

  # disaster recovery (docs/disaster-recovery.md) restores from the storagebox
  programs.ssh.knownHosts.storagebox = pkgs.lib.niveum.restic.knownHost;

  environment.systemPackages = with pkgs; [
    # desktop tools; mime.nix points at nsxiv and mupdf
    lf
    mupdf
    nsxiv
    poppler-utils
    yt-dlp
    exiftool
    qrencode
    magic-wormhole-rs
    rink
    comma
    gnome-disk-utility
    # installing niveum machines
    nixos-install-tools
    gptfdisk
    parted
    cryptsetup
    dosfstools
    ntfs3g
    smartmontools
    ddrescue
    testdisk
    # recovery and new machines
    age
    restic
    agenix
  ];
}
