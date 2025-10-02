{ pkgs, ... }:
{
  environment.systemPackages = [ pkgs.zeroad ];
  networking.firewall = {
    allowedTCPPorts = [ 20595 ];
    allowedUDPPorts = [ 20595 ];
  };
}
