{ lib, ... }:
let
  niveumLib = import <niveum/lib>;
  inherit (niveumLib) retiolumAddresses restic;
  firewall = niveumLib.firewall lib;
in
{
  services.restic.server = {
    enable = true;
    appendOnly = true;
    dataDir = "/backup/restic";
    prometheus = true;
    extraFlags = [ "--no-auth" ]; # auth is done via firewall
    listenAddress = ":${toString restic.port}";
  };

  networking.firewall =
  let
    dport = restic.port;
    protocol = "tcp";
    rules = [
      (firewall.accept { inherit dport protocol; source = retiolumAddresses.kabsa.ipv4; })
      (firewall.accept { inherit dport protocol; source = retiolumAddresses.manakish.ipv4; })
      (firewall.accept { inherit dport protocol; source = retiolumAddresses.makanek.ipv4; })
    ];
  in {
    extraCommands = firewall.addRules rules;
    extraStopCommands = firewall.removeRules rules;
  };
}
