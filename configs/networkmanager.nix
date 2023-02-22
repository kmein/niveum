{
  lib,
  pkgs,
  ...
}: let
  profile = name: custom:
    lib.recursiveUpdate {
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
    }
    custom;
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
in {
  imports = [../modules/networkmanager-declarative.nix];

  programs.nm-applet.enable = true;

  networking.networkmanager = {
    enable = true;
    plugins = [
      pkgs.networkmanager-openvpn
      pkgs.networkmanager-fortisslvpn
    ];
    wifi.macAddress = "random";
    ethernet.macAddress = "random";
    unmanaged = ["docker*"];
  };

  users.users.me.extraGroups = ["networkmanager"];

  environment.systemPackages = [
    pkgs.speedtest-cli
    pkgs.networkmanager-openvpn
    pkgs.networkmanagerapplet
    pkgs.networkmanager-fortisslvpn
  ];
}
