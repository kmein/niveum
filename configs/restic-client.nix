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

  services.restic.backups.niveum = lib.mkMerge [
    {
      initialize = true;
      inherit (repository) repository;
      extraOptions = [ (repository.sftpCommand config.age.secrets.restic-ssh.path) ];
      timerConfig = {
        OnCalendar = lib.mkDefault "daily";
        RandomizedDelaySec = "1h";
      };
      passwordFile = config.age.secrets.restic.path;
    }
    (lib.mkIf (restic.maintainer.${group} == config.networking.hostName) {
      inherit (restic) pruneOpts;
      checkOpts = [ "--with-cache" ];
    })
  ];

  # for BackupStale: ExecStartPost only runs once backup, prune and check have
  # all succeeded, so the timestamp means a snapshot actually landed
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
