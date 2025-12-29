{ config, lib, pkgs, ... }:
let
  sshPort = pkgs.lib.niveum.machines.${config.networking.hostName}.sshPort;
in
{
  services.tor.enable = true;
  services.tor.client.enable = true;
  environment.systemPackages = [
    pkgs.tor
    pkgs.torsocks
  ];

  services.tor.relay.onionServices = {
    "ssh" = {
      version = 3;
      map = [{
        port = sshPort;
        target.port = sshPort;
        target.addr = "127.0.0.1";
      }];
    };
  };
}
