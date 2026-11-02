{ config, pkgs, ... }:
let
  inherit (pkgs.lib.niveum) localAddresses;
in
{
  # mac randomisation rules out a dhcp reservation, so pin the address here.
  # the psk identifies the network; a bssid pin would tie us to one of its two radios
  age.secrets.wifi.file = ../secrets/wifi.age;

  networking.networkmanager.ensureProfiles = {
    environmentFiles = [ config.age.secrets.wifi.path ];
    profiles.Schilfpalast = {
      connection = {
        id = "Schilfpalast";
        type = "wifi";
      };
      wifi = {
        mode = "infrastructure";
        ssid = "Schilfpalast";
      };
      wifi-security = {
        key-mgmt = "wpa-psk";
        psk = "$schilfpalast";
      };
      ipv4 = {
        method = "manual";
        address1 = "${localAddresses.${config.networking.hostName}}/24,${localAddresses.router}";
        dns = "${localAddresses.router};";
      };
      ipv6.method = "auto";
    };
  };
}
