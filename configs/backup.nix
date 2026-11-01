{
  pkgs,
  config,
  lib,
  ...
}:
let
  repository = pkgs.lib.niveum.restic.repositories.personal;
  restic = ''${pkgs.restic}/bin/restic -r ${repository.repository} -o ${repository.sftpCommand config.age.secrets.restic-ssh.path} -p ${config.age.secrets.restic.path}'';
in
{
  imports = [ ./restic-client.nix ];

  # readable by me for the wrappers below
  age.secrets = {
    restic = {
      owner = config.users.users.me.name;
      group = config.users.users.me.group;
      mode = "400";
    };
    restic-ssh = {
      owner = config.users.users.me.name;
      group = config.users.users.me.group;
      mode = "400";
    };
  };

  services.restic.backups.niveum = {
    timerConfig.OnCalendar = "8:00";
    # a laptop suspending mid-backup drops the sftp connection
    inhibitsSleep = true;
    extraBackupArgs = [
      "--exclude=/home/kfm/sync/src/nixpkgs/.git"
      "--exclude=node_modules"
      "--exclude=.parcel-cache"
    ];
    paths = [
      "/home/kfm/sync"
      "/home/kfm/state"
      "/home/kfm/mobile"
      "/home/kfm/.gnupg"
      "/home/kfm/.electrum"
      "/home/kfm/.ssh"
    ];
  };

  systemd.services.restic-backups-niveum = {
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = "15s";
    };
    # don't try more than 4 times an hour; a failing run itself takes more than a
    # minute, so a narrower window would never be reached
    unitConfig = {
      StartLimitIntervalSec = "1h";
      StartLimitBurst = 4;
    };
  };

  environment.systemPackages = [
    (pkgs.writers.writeDashBin "restic-niveum" ''
      ${restic} "$@"
    '')
    (pkgs.writers.writeDashBin "restic-mount" ''
      mountdir=$(mktemp -p "$XDG_RUNTIME_DIR" -d "restic-mount-XXXXXXX")
      trap clean EXIT
      clean() {
        rm -r "$mountdir"
      }
      ${restic} mount "$mountdir"
    '')
  ];
}
