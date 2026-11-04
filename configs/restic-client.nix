{
  pkgs,
  config,
  lib,
  ...
}:
let
  inherit (pkgs.lib.niveum) restic textfileDirectory;
  group = restic.group.${config.networking.hostName};
  repository = restic.repositories.${group};
  maintainer = restic.maintainer.${group} == config.networking.hostName;
in
{
  age.secrets = {
    restic.file = ../secrets/restic-${group}.age;
    restic-ssh.file = ../secrets/restic-${group}-ssh.age;
  };

  programs.ssh.knownHosts.storagebox = restic.knownHost;

  services.restic.backups = {
    niveum = {
      initialize = true;
      inherit (repository) repository;
      extraOptions = [ (repository.sftpCommand config.age.secrets.restic-ssh.path) ];
      timerConfig = {
        OnCalendar = lib.mkDefault "daily";
        RandomizedDelaySec = "1h";
      };
      passwordFile = config.age.secrets.restic.path;
    };

    # a job of its own: prune needs an exclusive lock, which another host's backup
    # can hold for hours, and that must not fail (and restart) the backup itself
    niveum-maintenance = lib.mkIf maintainer {
      inherit (config.services.restic.backups.niveum) repository extraOptions passwordFile;
      pruneOpts = restic.pruneOpts ++ [ "--retry-lock 2h" ];
      checkOpts = [
        "--with-cache"
        "--retry-lock 2h"
      ];
      timerConfig = {
        OnCalendar = "04:00";
        RandomizedDelaySec = "1h";
        Persistent = true;
      };
    };
  };

  # Persistent catches a missed run up the moment a laptop resumes, before its
  # network is back; a run without network fails within a second. not on 11, a
  # lock still held after --retry-lock: that would just wait another two hours
  systemd.services.restic-backups-niveum-maintenance = lib.mkIf maintainer {
    serviceConfig = {
      Restart = "on-failure";
      RestartPreventExitStatus = 11;
      RestartSec = "5min";
    };
    # then give up until the next timer, so a lasting failure shows as failed
    unitConfig = {
      StartLimitIntervalSec = "1h";
      StartLimitBurst = 4;
    };
  };

  # for BackupStale: ExecStartPost only runs once the backup has succeeded, so the
  # timestamp means a snapshot actually landed
  systemd.services.restic-backups-niveum.serviceConfig.ExecStartPost =
    lib.mkIf config.services.prometheus.exporters.node.enable
      (
        pkgs.writeShellScript "restic-backup-timestamp" ''
          out=${textfileDirectory}/restic-backup.prom
          {
            echo "# HELP restic_backup_success_timestamp_seconds When the last restic backup succeeded."
            echo "# TYPE restic_backup_success_timestamp_seconds gauge"
            echo "restic_backup_success_timestamp_seconds $(date +%s)"
          } > "$out.tmp"
          mv "$out.tmp" "$out"
        ''
      );
}
