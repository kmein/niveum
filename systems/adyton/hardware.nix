# shared by adyton and its installer, so the installer is offline too
{
  config,
  lib,
  pkgs,
  ...
}:
let
  # R1: the build fails if any of these ends up in the kernel
  offline = [
    "INET"
    "PACKET"
    "NETDEVICES"
    "WIRELESS"
    "WLAN"
    "CFG80211"
    "BT"
    "NFC"
  ];
  firmware = config.hardware.raspberry-pi.firmware;
  # the firmware partition is not touched by switch-to-configuration
  installFirmware = pkgs.writers.writeDash "install-firmware" ''
    set -eu
    t=$1
    src=${firmware.package}/share/raspberrypi/boot
    cp "$src"/bootcode.bin "$src"/start*.elf "$src"/fixup*.dat "$src"/*.dtb "$t"/
    rm -rf "$t"/overlays
    cp -r "$src"/overlays "$t"/
    cp ${firmware.uboot.package}/u-boot.bin "$t"/u-boot.bin
    cp ${config.hardware.raspberry-pi.configtxt.file} "$t"/config.txt
  '';
in
{
  boot.kernelPackages = pkgs.linuxPackages_rpi3;
  boot.kernelPatches = [
    {
      name = "adyton";
      patch = null;
      structuredExtraConfig = lib.genAttrs offline (_: lib.mkForce lib.kernel.no) // {
        # the pi 3 has none; pci storage drivers would select NETDEVICES
        PCI = lib.mkForce lib.kernel.no;
        # R11: the pi 3 has no aes instructions
        CRYPTO_ADIANTUM = lib.mkForce lib.kernel.yes;
      };
    }
  ];
  system.checks = [
    (pkgs.runCommand "adyton-kernel-offline" { } ''
      for o in ${toString offline}; do
        if grep -Eq "^CONFIG_$o=[ym]" ${config.boot.kernelPackages.kernel.configfile}; then
          echo "CONFIG_$o is enabled" >&2
          exit 1
        fi
      done
      touch $out
    '')
  ];

  # R1: the radios are soldered on, so they can only be switched off
  hardware.raspberry-pi.configtxt.deviceTreeOverlays.all = [
    { vc4-kms-v3d = { }; }
    { disable-wifi = { }; }
    { disable-bt = { }; }
  ];
  hardware.firmware = lib.mkForce [ ];
  hardware.raspberry-pi.firmware.uboot.enable = true;
  system.systemBuilderCommands = "ln -s ${installFirmware} $out/install-firmware";

  networking.useDHCP = false;
  networking.firewall.enable = false;
  services.timesyncd.enable = false;

  boot.supportedFilesystems = lib.mkForce [
    "ext4"
    "vfat"
  ];
  boot.initrd.supportedFilesystems = lib.mkForce [
    "ext4"
    "vfat"
  ];

  # R8; the x86-only parameters of the threat model have no counterpart here
  boot.kernelParams = [
    "slab_nomerge"
    "slub_debug=FZ"
    "page_alloc.shuffle=1"
    "randomize_kstack_offset=on"
    "init_on_free=1"
  ];
  boot.kernel.sysctl = {
    "kernel.kptr_restrict" = 2;
    "kernel.kexec_load_disabled" = 1;
    "kernel.sysrq" = 0;
    "kernel.unprivileged_bpf_disabled" = 1;
    "net.core.bpf_jit_harden" = 2;
    # the maximum with 39-bit virtual addresses
    "vm.mmap_rnd_bits" = 24;
  };
  security.protectKernelImage = true;

  time.timeZone = "UTC";
}
