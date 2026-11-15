{
  config,
  lib,
  pkgs,
  ...
}:
let
  user = "kfm";
  # R7: the rest of $HOME is gone at power-off
  persisted = [
    ".gnupg"
    ".electrum"
    "vault"
  ];
  updateKey = ./update-key.pub;
  hasUpdateKey = builtins.pathExists updateKey;

  bind = source: {
    device = source;
    fsType = "none";
    # R10
    options = [
      "bind"
      "noexec"
      "nosuid"
      "nodev"
    ];
  };

  stick = label: options: {
    device = "/dev/disk/by-label/${label}";
    fsType = "vfat";
    # R3: mounted by hand only; `user` implies noexec,nosuid,nodev
    options = [
      "noauto"
      "user"
    ]
    ++ options;
  };

  update = pkgs.writers.writeBashBin "adyton-update" ''
    set -euo pipefail
    in=''${1:-/media/in}
    system=$(cat "$in/system")
    case $system in
      /nix/store/*-nixos-system-adyton-*) ;;
      *) echo "$in/system names no adyton system" >&2; exit 1 ;;
    esac
    # R2: require-sigs rejects every path the update key did not sign
    nix copy --from "file://$in/cache" "$system"
    nix-env -p /nix/var/nix/profiles/system --set "$system"
    mount /boot
    trap 'umount /boot' EXIT
    "$system/bin/switch-to-configuration" boot
    "$system/install-firmware" /boot
    nix-env -p /nix/var/nix/profiles/system --delete-generations +3
    nix-store --gc
    echo "updated; power off and on again"
  '';

  backup = pkgs.writers.writeBashBin "adyton-backup" ''
    set -euo pipefail
    export PATH=${
      lib.makeBinPath [
        pkgs.cryptsetup
        pkgs.e2fsprogs
        pkgs.rsync
      ]
    }:$PATH
    init=
    if [ "''${1:-}" = --init ]; then init=1; shift; fi
    dev=''${1:?usage: adyton-backup [--init] /dev/sdX}
    mnt=/run/adyton-backup
    if [ -n "$init" ]; then
      cryptsetup luksFormat --type luks2 --label adyton-backup \
        --cipher xchacha12,aes-adiantum-plain64 --key-size 256 \
        --pbkdf argon2id --pbkdf-memory 262144 --iter-time 4000 "$dev"
    fi
    cryptsetup open "$dev" adyton-backup
    trap 'umount -q "$mnt" || true; cryptsetup close adyton-backup' EXIT
    if [ -n "$init" ]; then mkfs.ext4 -q -L adyton-backup /dev/mapper/adyton-backup; fi
    mkdir -p "$mnt"
    mount -o noexec,nosuid,nodev /dev/mapper/adyton-backup "$mnt"
    rsync -a --delete /persist/ "$mnt/persist/"
    rm -f "$mnt/luks-header.img"
    cryptsetup luksHeaderBackup /dev/disk/by-label/adyton --header-backup-file "$mnt/luks-header.img"
    echo "backed up to $dev"
  '';
in
{
  imports = [
    ./hardware.nix
    {
      fileSystems = lib.listToAttrs (
        map (d: lib.nameValuePair "/home/${user}/${d}" (bind "/persist/${user}/${d}")) persisted
      );
    }
  ];

  networking.hostName = "adyton";
  system.stateVersion = "26.05";

  assertions = [
    {
      assertion = hasUpdateKey;
      message = "adyton: put the public update key into systems/adyton/update-key.pub and git add it (see systems/adyton/README.md)";
    }
  ];

  # R7: root holds only runtime state
  fileSystems."/" = {
    device = "none";
    fsType = "tmpfs";
    options = [
      "size=256M"
      "mode=755"
      "noexec"
      "nosuid"
      "nodev"
    ];
  };
  # R11: one passphrase at every boot opens lvm with both volumes
  boot.initrd.luks.devices.crypt.device = "/dev/disk/by-label/adyton";
  fileSystems."/nix" = {
    device = "/dev/adyton/nix";
    fsType = "ext4";
    options = [
      "noatime"
      "nosuid"
      "nodev"
    ];
  };
  # R10; needed for boot, since activation reads the password hashes
  fileSystems."/persist" = {
    device = "/dev/adyton/persist";
    fsType = "ext4";
    neededForBoot = true;
    options = [
      "noatime"
      "noexec"
      "nosuid"
      "nodev"
    ];
  };
  # mounted only by adyton-update
  fileSystems."/boot" = {
    device = "/dev/disk/by-label/ADYTONBOOT";
    fsType = "vfat";
    options = [
      "noauto"
      "noexec"
      "nosuid"
      "nodev"
      "umask=0077"
    ];
  };
  fileSystems."/var/lib/usbguard" = bind "/persist/usbguard";
  fileSystems."/media/in" = stick "IN" [ "ro" ];
  fileSystems."/media/out" = stick "OUT" [
    "uid=${toString config.users.users.${user}.uid}"
    "gid=100"
    "umask=077"
  ];
  systemd.tmpfiles.rules = [
    "d /media/in 0755 root root -"
    "d /media/out 0755 root root -"
  ];
  boot.loader.generic-extlinux-compatible.configurationLimit = 3;

  # R7
  swapDevices = [ ];
  systemd.sleep.settings.Sleep = {
    AllowSuspend = false;
    AllowHibernation = false;
    AllowHybridSleep = false;
    AllowSuspendThenHibernate = false;
  };
  services.journald.storage = "volatile";

  # R2
  nix.channel.enable = false;
  nix.settings = {
    substituters = lib.mkForce [ ];
    trusted-public-keys = lib.mkForce (lib.optional hasUpdateKey (lib.fileContents updateKey));
    require-sigs = true;
    # R10: a user build would put new executables into the store
    allowed-users = [ "root" ];
    trusted-users = [ "root" ];
  };

  # R9
  users.mutableUsers = false;
  users.users.root.hashedPassword = "!";
  users.users.admin = {
    isNormalUser = true;
    uid = 1001;
    extraGroups = [ "wheel" ];
    hashedPasswordFile = "/persist/secrets/admin.hash";
  };
  users.users.${user} = {
    isNormalUser = true;
    uid = 1000;
    hashedPasswordFile = "/persist/secrets/${user}.hash";
  };
  security.sudo.execWheelOnly = true;

  # R3
  services.usbguard = {
    enable = true;
    implicitPolicyTarget = "block";
    presentDevicePolicy = "apply-policy";
    insertedDevicePolicy = "apply-policy";
  };
  # R3, R8: nothing can be loaded once a stick could be attached
  security.lockKernelModules = true;
  boot.kernelModules = [
    "usb_storage"
    "uas"
    "vfat"
    "nls_cp437"
    "nls_iso8859_1"
    "dm_crypt"
  ];

  # there is no rtc: without this, keys and signatures carry a made-up date
  systemd.services.ask-date = {
    wantedBy = [ "multi-user.target" ];
    before = [ "getty@tty1.service" ];
    serviceConfig = {
      Type = "oneshot";
      StandardInput = "tty";
      StandardOutput = "tty";
      TTYPath = "/dev/tty1";
      TTYReset = true;
    };
    script = ''
      while :; do
        printf 'UTC date and time (YYYY-MM-DD HH:MM): '
        read -r t
        [ -n "$t" ] && date -u -s "$t" && break
      done
    '';
  };

  programs.gnupg.agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-curses;
  };
  environment.systemPackages = [
    pkgs.age
    pkgs.cryptsetup
    pkgs.electrum
    pkgs.mat2 # R5
    pkgs.paperkey
    update
    backup
  ];
}
