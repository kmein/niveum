{ config, pkgs, lib, ... }:
let
  radioStations = import <niveum/lib/radio-stations.nix>;
  radioStationsFile = pkgs.writeText "stations" (lib.concatStringsSep "\n" radioStations);
in
{
  system.activationScripts.webradio = ''
    install -d /var/lib/mpd/playlists
    ln -sfn ${toString radioStationsFile} /var/lib/mpd/playlists/webradio.m3u
  '';

  services.mpd.enable = true;
  services.ympd.enable = true;

  # dont let anyone outside localhost or local network in
  networking.firewall.extraCommands = let ympdPort = config.services.ympd.webPort; in ''
    ${pkgs.iptables}/bin/iptables -A INPUT -p tcp --dport ${ympdPort} -s 192.168.0.0/16 -j ACCEPT
    ${pkgs.iptables}/bin/iptables -A INPUT -p tcp --dport ${ympdPort} -s 127.0.0.0/8 -j ACCEPT
    ${pkgs.iptables}/bin/iptables -A INPUT -p tcp --dport ${ympdPort} -j DROP
  '';
}
