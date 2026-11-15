# nix build '.?submodules=1#nixosConfigurations.adyton-installer.config.system.build.sdImage'
{
  config,
  lib,
  pkgs,
  modulesPath,
  adyton,
  ...
}:
let
  system = adyton.config.system.build.toplevel;
  users = adyton.config.users.users;

  persistDirs = lib.filter (lib.hasPrefix "/persist/") (
    map (fs: fs.device) (lib.attrValues adyton.config.fileSystems)
  );
  # /persist/<user>/… belongs to that user
  owner =
    dir:
    let
      name = lib.elemAt (lib.splitString "/" dir) 2;
    in
    if users ? ${name} then toString users.${name}.uid else "0";
  passwordUsers = lib.filter (u: users.${u}.hashedPasswordFile != null) (lib.attrNames users);

  install = pkgs.writers.writeBashBin "adyton-install" ''
    set -euo pipefail
    export PATH=${
      lib.makeBinPath [
        pkgs.cryptsetup
        pkgs.dosfstools
        pkgs.e2fsprogs
        pkgs.lvm2
        pkgs.mkpasswd
        pkgs.usbguard
        pkgs.util-linux
      ]
    }:$PATH
    dev=''${1:?usage: adyton-install /dev/sdX}
    if [ "$(lsblk -no PKNAME "$(findmnt -no SOURCE /)")" = "$(basename "$dev")" ]; then
      echo "$dev holds this installer" >&2
      exit 1
    fi
    lsblk "$dev"
    read -rp "erase $dev? type yes: " ok
    [ "$ok" = yes ]

    # the pi 3 boot rom reads only mbr
    sfdisk --wipe always "$dev" <<EOF
    label: dos
    size=1GiB, type=c, bootable
    type=83
    EOF
    udevadm settle
    mapfile -t parts < <(lsblk -lnpo NAME,TYPE "$dev" | awk '$2 == "part" { print $1 }')

    mkfs.vfat -F 32 -n ADYTONBOOT "''${parts[0]}"
    # R11; adiantum since the pi 3 has no aes instructions
    cryptsetup luksFormat --type luks2 --label adyton \
      --cipher xchacha12,aes-adiantum-plain64 --key-size 256 \
      --pbkdf argon2id --pbkdf-memory 262144 --iter-time 4000 "''${parts[1]}"
    cryptsetup open "''${parts[1]}" crypt
    pvcreate /dev/mapper/crypt
    vgcreate adyton /dev/mapper/crypt
    lvcreate -L 2G -n persist adyton
    lvcreate -l 100%FREE -n nix adyton
    mkfs.ext4 -q -L nix /dev/adyton/nix
    mkfs.ext4 -q -L persist /dev/adyton/persist

    mount -t tmpfs -o mode=755 none /mnt
    mkdir /mnt/nix /mnt/persist /mnt/boot
    mount /dev/adyton/nix /mnt/nix
    mount /dev/adyton/persist /mnt/persist
    mount "''${parts[0]}" /mnt/boot

    install -d -m 700 /mnt/persist/secrets
    ${lib.concatMapStrings (d: ''
      install -d -m 700 -o ${owner d} -g ${if owner d == "0" then "0" else "100"} /mnt${d}
    '') persistDirs}

    for u in ${toString passwordUsers}; do
      while :; do
        read -rsp "password for $u: " a; echo
        read -rsp "again: " b; echo
        [ -n "$a" ] && [ "$a" = "$b" ] && break
        echo "empty or not the same" >&2
      done
      printf '%s\n' "$a" | mkpasswd -m yescrypt --stdin > /mnt/persist/secrets/$u.hash
    done
    chmod 600 /mnt/persist/secrets/*

    # R3: whatever is attached now is allowed from now on
    read -rp "attach the keyboard and every stick adyton should accept, then press enter "
    usbguard generate-policy > /mnt/persist/usbguard/rules.conf
    chmod 600 /mnt/persist/usbguard/rules.conf

    nixos-install --root /mnt --system ${system} --no-root-passwd --no-channel-copy
    ${system}/install-firmware /mnt/boot

    umount /mnt/boot /mnt/persist /mnt/nix /mnt
    vgchange -an adyton
    cryptsetup close crypt
    echo "done: power off, wipe this card, boot from $dev"
  '';
in
{
  imports = [
    ./hardware.nix
    "${modulesPath}/installer/sd-card/sd-image.nix"
  ];

  networking.hostName = "adyton-installer";
  system.stateVersion = "26.05";

  sdImage.storePaths = [ system ];
  sdImage.populateRootCommands = ''
    mkdir -p ./files/boot
    ${config.boot.loader.generic-extlinux-compatible.populateCmd} -c ${config.system.build.toplevel} -d ./files/boot
  '';

  # holds nothing secret; it only formats the real card
  services.getty.autologinUser = "root";
  environment.systemPackages = [ install ];
}
