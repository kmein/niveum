{ pkgs, system }:
pkgs.writeShellApplication {
  name = "kaak-install";
  runtimeInputs = with pkgs; [
    util-linux
    gptfdisk
    parted
    systemd
    cryptsetup
    dosfstools
    e2fsprogs
    nixos-install-tools
    nix
    coreutils
    gawk
  ];
  text = ''
    disk=''${1:?usage: kaak-install /dev/sdX}

    # only partitions on $disk: names differ between sdX1, nvme0n1p1 and loop0p1,
    # and other disks may carry the same labels
    part() { lsblk -lnpo NAME,PARTLABEL "$disk" | awk -v l="$1" '$2 == l { print $1 }'; }

    # the system is already built by the caller; only disk access needs root
    run() { sudo env PATH="$PATH" "$@"; }

    fresh=
    if [ -z "$(part kaak-crypt)" ]; then
      lsblk "$disk"
      read -rp "wipe $disk? type its path again: " answer
      if [ "$answer" != "$disk" ]; then
        echo "not wiping" >&2
        exit 1
      fi
      run sgdisk --zap-all "$disk"
      run sgdisk -n1:0:+1G -t1:ef00 -c1:kaak-boot -n2:0:0 -t2:8309 -c2:kaak-crypt "$disk"
      run partprobe "$disk"
      run udevadm settle
      run mkfs.vfat -F 32 -n KAAK-BOOT "$(part kaak-boot)"
      run cryptsetup luksFormat --type luks2 "$(part kaak-crypt)"
      fresh=1
    fi

    run cryptsetup open "$(part kaak-crypt)" kaak-install
    mnt=$(mktemp -d)
    cleanup() {
      run umount -R "$mnt" || true
      run cryptsetup close kaak-install || true
      rmdir "$mnt"
    }
    trap cleanup EXIT
    if [ -n "$fresh" ]; then
      run mkfs.ext4 -L kaak /dev/mapper/kaak-install
    fi

    run mount -o noatime /dev/mapper/kaak-install "$mnt"
    run mkdir -p "$mnt/boot"
    run mount -o umask=0077 "$(part kaak-boot)" "$mnt/boot"

    # on an existing root this adds a generation
    run nixos-install --system ${system} --root "$mnt" --no-root-passwd --no-channel-copy
  '';
}
