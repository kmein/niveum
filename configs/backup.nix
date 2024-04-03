{
  pkgs,
  config,
  ...
}: let
  inherit (import ../lib) restic;
in {
  services.restic.backups.niveum = {
    initialize = true;
    inherit (restic) repository;
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
      "/home/kfm/.gnupg"
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
      ${pkgs.restic}/bin/restic -r ${restic.repository} -p ${config.age.secrets.restic.path} "$@"
    '')
    (pkgs.writers.writeDashBin "restic-mount" ''
      mountdir=$(mktemp -d)
      trap clean EXIT
      clean() {
        rm -r "$mountdir"
      }
      ${pkgs.restic}/bin/restic -r ${restic.repository} -p ${config.age.secrets.restic.path} mount "$mountdir"
    '')
  ];
}
