{ pkgs, ... }:
{
  environment.systemPackages = [
    pkgs.zeroad
    pkgs.mari0
    pkgs.luanti # fka minetest
  ];
  networking.firewall = {
    # for 0ad multiplayer
    allowedTCPPorts = [ 20595 ];
    allowedUDPPorts = [ 20595 ];
  };
}
