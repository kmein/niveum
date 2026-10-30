{ pkgs, ... }:
{
  environment.systemPackages = [
    pkgs.mari0
    # pkgs.luanti # fka minetest
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
    # for 0ad multiplayer; 0ad itself is in the nix profile, as its data alone would fill the cachix cache
    allowedTCPPorts = [ 20595 ];
    allowedUDPPorts = [ 20595 ];
  };
}
