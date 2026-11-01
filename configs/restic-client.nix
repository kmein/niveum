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
    niveum-maintenance = lib.mkIf (restic.maintainer.${group} == config.networking.hostName) {
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
