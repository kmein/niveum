{
  config,
  pkgs,
  lib,
  ...
}:
let
  dataDir = "/backup/restic";
in
{
  services.restic.server = {
    enable = true;
    appendOnly = true;
    inherit dataDir;
    prometheus = true;
    extraFlags = [ "--no-auth" ]; # auth is done via firewall
    listenAddress = toString pkgs.lib.niveum.restic.port;
  };

  services.prometheus.exporters.restic = {
    enable = true;
    # over loopback rather than pkgs.lib.niveum.restic.repository, which would take
    # the long way around through tinc and is not covered by the rules below.
    # not the local path either: that would mean turning off the module's DynamicUser
    repository = "rest:http://127.0.0.1:${toString pkgs.lib.niveum.restic.port}/";
    passwordFile = config.age.secrets.restic.path; # read by systemd as root, via LoadCredential
    # with the two expensive calls disabled a refresh is just `snapshots` + `locks`
    refreshInterval = 1800;
    openFirewall = true; # scraped from makanek, like the node exporter
  };

  # The exporter does its whole refresh inside __init__, i.e. *before* it starts
  # the HTTP server, so anything slow in there means it never becomes scrapeable.
  # On this repository (684 GB, 721 snapshots) both of these are far too slow:
  # `stats` runs once per snapshot (53 min CPU, 3.1 GB of HTTP traffic) and `check`
  # took over 15 minutes. Snapshot timestamps, which is what BackupStale needs,
  # come from the snapshot list and are unaffected. Integrity checking moved to
  # the weekly timer below; restic_backup_size_total and _files_total are lost.
  systemd.services.prometheus-restic-exporter.environment = {
    NO_STATS = "true";
    NO_CHECK = "true";
  };

  # `restic check` where it belongs: on a schedule that suits the repository size,
  # reported through node_exporter's textfile collector rather than blocking a scrape
  systemd.services.restic-check = {
    description = "Verify the restic repository";
    startAt = "Wed 03:00"; # not Sunday: the offsite mirror runs then
    environment.RESTIC_CACHE_DIR = "/var/cache/restic-check";
    serviceConfig = {
      Type = "oneshot";
      CacheDirectory = "restic-check";
      # reads the whole repository; stay out of the way of everything else
      Nice = 19;
      IOSchedulingClass = "idle";
      ReadWritePaths = [ pkgs.lib.niveum.textfileDirectory ];
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      NoNewPrivileges = true;
    };
    script = ''
      out=${pkgs.lib.niveum.textfileDirectory}/restic-check.prom
      start=$(date +%s)
      # the local path, not the rest-server: no HTTP round trip per pack file
      if ${lib.getExe pkgs.restic} -r ${dataDir} -p ${config.age.secrets.restic.path} --no-lock check; then
        success=1
      else
        success=0
      fi
      end=$(date +%s)
      {
        echo "# HELP restic_repo_check_success Whether the last restic check of the backup repository succeeded."
        echo "# TYPE restic_repo_check_success gauge"
        echo "restic_repo_check_success $success"
        echo "# HELP restic_repo_check_timestamp_seconds When the last restic check finished."
        echo "# TYPE restic_repo_check_timestamp_seconds gauge"
        echo "restic_repo_check_timestamp_seconds $end"
        echo "# HELP restic_repo_check_duration_seconds How long the last restic check took."
        echo "# TYPE restic_repo_check_duration_seconds gauge"
        echo "restic_repo_check_duration_seconds $((end - start))"
      } > "$out.tmp"
      mv "$out.tmp" "$out"
    '';
  };

  systemd.timers.restic-check.timerConfig.RandomizedDelaySec = "1h";

  environment.systemPackages = [
    (pkgs.writers.writeDashBin "restic-niveum" ''
      exec ${pkgs.util-linux}/bin/runuser -u restic -g restic -- ${pkgs.restic}/bin/restic -r ${toString dataDir} -p ${config.age.secrets.restic.path} "$@"
    '')
  ];

  fileSystems."/backup" = {
    device = "/dev/disk/by-id/ata-WDC_WD10JPVX-22JC3T0_WD-WXD1E5510MKW";
    fsType = "ext4";
  };

  networking.firewall =
    let
      dport = pkgs.lib.niveum.restic.port;
      protocol = "tcp";
      rules =
        map
          (
            host:
            pkgs.lib.niveum.firewall.accept {
              inherit dport protocol;
              source = pkgs.lib.niveum.retiolumAddresses.${host}.ipv4;
            }
          )
          [
            "kabsa"
            "manakish"
            "makanek"
            "fatteh"
            "ful"
          ];
    in
    {
      extraCommands = pkgs.lib.niveum.firewall.addRules rules;
      extraStopCommands = pkgs.lib.niveum.firewall.removeRules rules;
    };

  ## offsite backup

  systemd.services.restic-rsync-offsite = {
    description = "Mirror restic repo offsite";
    script = ''
      ${lib.getExe pkgs.rsync} \
        --rsh=${pkgs.writers.writeDash "rsh" ''
          ${lib.getExe pkgs.openssh} \
            -i ${config.age.secrets.zaatar-khall-restic-ssh.path} \
            -p ${toString pkgs.lib.niveum.machines.khall.sshPort} \
            -o StrictHostKeyChecking=yes \
            -o UserKnownHostsFile=${pkgs.writeText "known_hosts" ''
              khall.hyprspace ${pkgs.lib.niveum.machines.khall.hostKey}
            ''} \
            "$@"
        ''} \
        --archive \
        --hard-links \
        --delete-delay \
        --numeric-ids \
        --info=progress2 \
        ${dataDir}/ \
        restic-backup@khall.hyprspace:/mnt/backup/restic-repo/
    '';

    startAt = "Sun 04:00";

    serviceConfig = {
      Type = "oneshot";
      User = "restic";
      Group = "restic";
      PrivateTmp = true;
      ProtectSystem = "strict";
      ProtectHome = true;
      NoNewPrivileges = true;
    };
  };

  systemd.timers.restic-rsync-offsite.timerConfig.RandomizedDelaySec = "1h";
}
