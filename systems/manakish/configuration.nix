{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (import ../../lib) retiolumAddresses;
in {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./hdmi.nix
    ../../configs/default.nix
    ../../configs/tlp.nix
    ../../configs/wpa_supplicant.nix
    ../../configs/admin-essentials.nix
  ];

  age.secrets = {
    retiolum-rsa = {
      file = ../../secrets/manakish-retiolum-privateKey-rsa.age;
      mode = "400";
      owner = "tinc.retiolum";
      group = "tinc.retiolum";
    };
    retiolum-ed25519 = {
      file = ../../secrets/manakish-retiolum-privateKey-ed25519.age;
      mode = "400";
      owner = "tinc.retiolum";
      group = "tinc.retiolum";
    };
    syncthing-cert.file = ../../secrets/manakish-syncthing-cert.age;
    syncthing-key.file = ../../secrets/manakish-syncthing-key.age;
  };

  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/gruvbox-light-medium.yaml";

  niveum = {
    batteryName = "BAT0";
    wirelessInterface = "wlp3s0";
    promptColours.success = "green";
  };

  networking = {
    useDHCP = false;
    interfaces = {
      enp0s25.useDHCP = true;
      wlp3s0.useDHCP = true;
      wwp0s20u4i6.useDHCP = true;
    };
    wireless.interfaces = ["wlp3s0"];
    retiolum = retiolumAddresses.manakish;
    hostName = "manakish";
  };

  systemd.services.systemd-networkd-wait-online.enable = lib.mkForce false;

  system.stateVersion = "20.09"; # Did you read the comment?
}
