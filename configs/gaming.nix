{ pkgs, ... }:
{
  environment.systemPackages = [
    pkgs.zeroad
    pkgs.mari0
    pkgs.luanti # fka minetest
    # pkgs.openarena
    # pkgs.teeworlds
    pkgs.nethack
    # pkgs.freeciv
    # pkgs.lincity-ng
    # pkgs.superTuxKart

    pkgs.morris
    pkgs.gnome-chess
    pkgs.gnuchess
  ];
  networking.firewall = {
    # for 0ad multiplayer
    allowedTCPPorts = [ 20595 ];
    allowedUDPPorts = [ 20595 ];
  };
}
