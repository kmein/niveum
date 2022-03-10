{
  pkgs,
  lib,
  ...
}: let
  niveumLib = import <niveum/lib>;
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
    listenAddress = ":${toString restic.port}";
  };

  environment.systemPackages = [
    (pkgs.writers.writeDashBin "restic-niveum" ''
      exec ${pkgs.util-linux}/bin/runuser -u restic -g restic -- ${pkgs.restic}/bin/restic -r ${toString dataDir} -p ${<secrets/restic/password>} "$@"
    '')
  ];

  fileSystems."/backup" = {
    device = "/dev/disk/by-id/ata-ST500LM021-1KJ152_W626LS9M";
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
    ];
  in {
    extraCommands = firewall.addRules rules;
    extraStopCommands = firewall.removeRules rules;
  };
}
