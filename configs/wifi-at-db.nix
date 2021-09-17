{ lib, ... }:
let
  ssids = [ "WIFIonICE" "WIFI@DB" ];
in
{
  # ref https://gist.github.com/sunsided/7840e89ff4e11b64a2d7503fafa0290c
  virtualisation.docker.extraOptions = lib.concatStringsSep " " [
    "--bip=172.39.1.5/24"
    "--fixed-cidr=172.39.1.0/25"
  ];

  networking.wireless.networks = lib.listToAttrs (map (ssid: {name = ssid; value = {};}) ssids);

  # fix dns
  systemd.network.networks.wifi-at-db = {
    networkConfig.DHCP = "yes";
    matchConfig.Name = "wlp3s0";
    matchConfig.SSID = lib.concatStringsSep " " ssids;
  };
}
