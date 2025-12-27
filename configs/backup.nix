{
  pkgs,
  config,
  lib,
  ...
}:
{
  services.restic.backups.niveum = {
    initialize = true;
    repository = pkgs.lib.niveum.restic.repository;
    timerConfig = {
      OnCalendar = "8:00";
      RandomizedDelaySec = "1h";
    };
    passwordFile = config.age.secrets.restic.path;
    extraBackupArgs = [
      "--exclude=/home/kfm/sync/src/nixpkgs/.git"
      "--exclude=node_modules"
      "--exclude=.parcel-cache"
    ];
    paths = [
      "/home/kfm/sync"
      "/home/kfm/state"
      "/home/kfm/cloud"
      "/home/kfm/mobile"
      "/home/kfm/.gnupg"
      "/home/kfm/.electrum"
      "/home/kfm/.ssh"
    ];
  };

  systemd.services.restic-backups-niveum.serviceConfig = {
    Restart = "on-failure";
    RestartSec = "15s";
    StartLimitIntervalSec = "1m"; # don't try more than 4 times
    StartLimitBurst = 4;
  };

  environment.systemPackages = [
    (pkgs.writers.writeDashBin "restic-niveum" ''
      ${pkgs.restic}/bin/restic -r ${pkgs.lib.niveum.restic.repository} -p ${config.age.secrets.restic.path} "$@"
    '')
    (pkgs.writers.writeDashBin "restic-mount" ''
      mountdir=$(mktemp -p "$XDG_RUNTIME_DIR" -d "restic-mount-XXXXXXX")
      trap clean EXIT
      clean() {
        rm -r "$mountdir"
      }
      ${pkgs.restic}/bin/restic -r ${pkgs.lib.niveum.restic.repository} -p ${config.age.secrets.restic.path} mount "$mountdir"
    '')
  ];
}
