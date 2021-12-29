{ lib, pkgs, ... }:
let
  profile = name: custom: lib.recursiveUpdate {
    connection.id = name;
    connection.type = "wifi";
    connection.interface-name = "wlp3s0";
    connection.permissions = "";
    wifi.mac-address-blacklist = "";
    wifi.ssid = name;
    wifi.mode = "infrastructure";
    ipv4.dns-search = "";
    ipv4.method = "auto";
    ipv6.addr-gen-mode = "stable-privacy";
    ipv6.dns-search = "";
    ipv6.method = "auto";
    proxy = {};
  } custom;
  eduroamProfile = {
    connection.uuid = "eae9fee6-a7d2-4120-a609-440b457d6fcf";
    wifi-security = {
      group = "ccmp;tkip;";
      key-mgmt = "wpa-eap";
      pairwise = "ccmp;";
      proto = "rsn;";
    };
    "802-1x" = {
      altsubject-matches = "DNS:srv1-radius.cms.hu-berlin.de;DNS:srv2-radius.cms.hu-berlin.de;";
      anonymous-identity = "anonymous@wlan.hu-berlin.de";
      ca-cert = pkgs.fetchurl {
        url = "https://www.cms.hu-berlin.de/de/dl/netze/wlan/config/eduroam/t-telesec_globalroot_class_2.pem";
        sha256 = "0if8aqd06sid7a0vw009zpa087wxcgdd2x6z2zs4pis5kvyqj2dk";
      };
      eap = "ttls;";
      identity = lib.strings.fileContents <secrets/eduroam/identity>;
      password = lib.strings.fileContents <secrets/eduroam/password>;
      phase2-auth = "pap";
    };
  };
in
{
  imports = [ ../modules/networkmanager-declarative.nix ];

  networking.networkmanager = {
    enable = true;
    wifi.macAddress = "random";
    ethernet.macAddress = "random";
    unmanaged = [ "docker*" ];
    profiles = lib.mapAttrs profile {
      Aether = {
        connection.uuid = "7138bb0f-1aeb-4905-890e-a6628427aa21";
        ipv6.addr-gen-mode = "stable";
        wifi.cloned-mac-address = "stable";
        wifi-security = {
          psk = lib.strings.fileContents <secrets/wifi/Aether.psk>;
          auth-alg = "open";
          key-mgmt = "wpa-psk";
        };
      };
      FactoryCommunityGuest = {
        connection.uuid = "fb1f2e52-651e-48b5-a72c-1accddf31afb";
        connection.timestamp = "1631885129";
        wifi.seen-bssids = "54:EC:2F:19:30:DC;54:EC:2F:19:5C:9C;54:EC:2F:58:E4:3C;";
        wifi-security = {
          psk = "Factory4ever";
          auth-alg = "open";
          key-mgmt = "wpa-psk";
        };
      };
      o2-WLAN66 = {
        connection.uuid = "c563aec3-f344-4ffb-8d1c-60a6cdac8fe0";
        wifi-security = {
          psk = "PK3468KV488T934U";
          auth-alg = "open";
          key-mgmt = "wpa-psk";
        };
      };
      "WIFI@DB".connection.uuid = "4eff4e94-8850-4e9f-a338-1787d0d90479";
      eduroam = eduroamProfile;
      eduroam_5GHz = eduroamProfile;
    };
  };

  users.users.me.extraGroups = [ "networkmanager" ];

  environment.systemPackages = [ pkgs.speedtest-cli ];
}
