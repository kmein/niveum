{
  pkgs,
  config,
  ...
}: let
  inherit (import <niveum/lib>) restic;
in {
  services.restic.backups.niveum = {
    initialize = true;
    inherit (restic) repository;
    timerConfig = {
      OnCalendar = "8:00";
      RandomizedDelaySec = "1h";
    };
    passwordFile = toString <secrets/restic/password>;
    extraBackupArgs = [
      "--exclude=/home/kfm/projects/nixpkgs/.git"
      "--exclude=node_modules"
    ];
    paths = [
      "/home/kfm/work"
      "/home/kfm/projects"
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
      ${pkgs.restic}/bin/restic -r ${restic.repository} -p ${<secrets/restic/password>} "$@"
    '')
    (pkgs.writers.writeDashBin "restic-mount" ''
      mountdir=$(mktemp -d)
      trap clean EXIT
      clean() {
        rm -r "$mountdir"
      }
      ${pkgs.restic}/bin/restic -r ${restic.repository} -p ${<secrets/restic/password>} mount "$mountdir"
    '')
  ];
}
