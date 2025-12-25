{
  config,
  pkgs,
  lib,
  ...
}: let
  dataDir = "/backup/restic";
in {
  services.restic.server = {
    enable = true;
    appendOnly = true;
    inherit dataDir;
    prometheus = true;
    extraFlags = ["--no-auth"]; # auth is done via firewall
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

  networking.firewall = let
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
  in {
    extraCommands = pkgs.lib.niveum.firewall.addRules rules;
    extraStopCommands = pkgs.lib.niveum.firewall.removeRules rules;
  };
}
