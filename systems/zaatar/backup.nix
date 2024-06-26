{
  config,
  pkgs,
  lib,
  ...
}: let
  niveumLib = import ../../lib;
  inherit (niveumLib) retiolumAddresses restic;
  firewall = niveumLib.firewall lib;
  dataDir = "/backup/restic";
in {
  services.restic.server = {
    enable = true;
    appendOnly = true;
    inherit dataDir;
    prometheus = true;
    extraFlags = ["--no-auth"]; # auth is done via firewall
    listenAddress = toString restic.port;
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
    dport = restic.port;
    protocol = "tcp";
    rules = [
      (firewall.accept {
        inherit dport protocol;
        source = retiolumAddresses.kabsa.ipv4;
      })
      (firewall.accept {
        inherit dport protocol;
        source = retiolumAddresses.manakish.ipv4;
      })
      (firewall.accept {
        inherit dport protocol;
        source = retiolumAddresses.makanek.ipv4;
      })
      (firewall.accept {
        inherit dport protocol;
        source = retiolumAddresses.fatteh.ipv4;
      })
      (firewall.accept {
        inherit dport protocol;
        source = retiolumAddresses.ful.ipv4;
      })
    ];
  in {
    extraCommands = firewall.addRules rules;
    extraStopCommands = firewall.removeRules rules;
  };
}
