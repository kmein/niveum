{ pkgs, ... }:
{
  networking.hostName = "khall";
  networking.networkmanager.enable = true;
  system.stateVersion = "25.11";

  fileSystems."/mnt/backup" = {
    device = "/dev/disk/by-uuid/9ae83b1a-6b5d-4c13-ae16-414c4539ccba";
    fsType = "ext4";
    options = [
      "noatime"
      "nofail"
    ];
  };

  environment.systemPackages = with pkgs; [
    rsync
  ];

  users.users.restic-backup = {
    isSystemUser = true;
    group = "restic-backup";
    home = "/var/lib/restic-backup";
    createHome = true;
    shell = pkgs.bash; # no interactive shell
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINgi6AC0FDyqzrHuAMSGKfNbT/T7lwykcYxxQMRihZCa" # zaatar-khall-restic-ssh
    ];
  };
  users.groups.restic-backup = { };
}
