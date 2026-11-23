{ pkgs, system }:
pkgs.writeShellApplication {
  name = "mouneh-install";
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
    disk=''${1:?usage: mouneh-install /dev/sdX}

    # only partitions on $disk: names differ between sdX1, nvme0n1p1 and loop0p1,
    # and other disks may carry the same labels
    part() { lsblk -lnpo NAME,PARTLABEL "$disk" | awk -v l="$1" '$2 == l { print $1 }'; }

    # the system is already built by the caller; only disk access needs root
    run() { sudo env PATH="$PATH" "$@"; }

    if [ -z "$(part mouneh-crypt)" ]; then
      lsblk "$disk"
      read -rp "wipe $disk? type its path again: " answer
      if [ "$answer" != "$disk" ]; then
        echo "not wiping" >&2
        exit 1
      fi
      run sgdisk --zap-all "$disk"
      run sgdisk -n1:0:+1G -t1:ef00 -c1:mouneh-boot -n2:0:0 -t2:8309 -c2:mouneh-crypt "$disk"
      run partprobe "$disk"
      run udevadm settle
    fi

    # by content, not by label: an interrupted run leaves labelled but empty partitions
    fstype() { run blkid -p -o value -s TYPE "$1" || true; }
    if [ -z "$(fstype "$(part mouneh-boot)")" ]; then
      run mkfs.vfat -F 32 -n MOUNEH-BOOT "$(part mouneh-boot)"
    fi
    if ! run cryptsetup isLuks "$(part mouneh-crypt)"; then
      run cryptsetup luksFormat --type luks2 "$(part mouneh-crypt)"
    fi

    run cryptsetup open "$(part mouneh-crypt)" mouneh-install
    mnt=$(mktemp -d)
    cleanup() {
      run umount -R "$mnt" || true
      run cryptsetup close mouneh-install || true
      rmdir "$mnt"
    }
    trap cleanup EXIT
    if [ -z "$(fstype /dev/mapper/mouneh-install)" ]; then
      run mkfs.ext4 -L mouneh /dev/mapper/mouneh-install
    fi

    run mount -o noatime /dev/mapper/mouneh-install "$mnt"
    run mkdir -p "$mnt/boot"
    run mount -o umask=0077 "$(part mouneh-boot)" "$mnt/boot"

    # on an existing root this adds a generation
    run nixos-install --system ${system} --root "$mnt" --no-root-passwd --no-channel-copy
  '';
}
