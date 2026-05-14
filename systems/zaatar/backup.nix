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
      rules = [
        (pkgs.lib.niveum.firewall.accept {
          inherit dport protocol;
          source = pkgs.lib.niveum.retiolumAddresses.kabsa.ipv4;
        })
        (pkgs.lib.niveum.firewall.accept {
          inherit dport protocol;
          source = pkgs.lib.niveum.retiolumAddresses.manakish.ipv4;
        })
        (pkgs.lib.niveum.firewall.accept {
          inherit dport protocol;
          source = pkgs.lib.niveum.retiolumAddresses.makanek.ipv4;
        })
        (pkgs.lib.niveum.firewall.accept {
          inherit dport protocol;
          source = pkgs.lib.niveum.retiolumAddresses.fatteh.ipv4;
        })
        (pkgs.lib.niveum.firewall.accept {
          inherit dport protocol;
          source = pkgs.lib.niveum.retiolumAddresses.ful.ipv4;
        })
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
