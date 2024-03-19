{
  config,
  pkgs,
  ...
}: let
  inherit (import ../../lib) retiolumAddresses;
in {
  imports = [
    ./hardware-configuration.nix
    ../../configs/networkmanager.nix
    ../../configs/default.nix
    # ../../configs/gnome.nix
  ];

  niveum = {
    batteryName = "BAT1";
    wirelessInterface = "wlp3s0";
    promptColours.success = "blue";
  };

  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/onedark.yaml";

  age.secrets = {
    retiolum-rsa = {
      file = ../../secrets/fatteh-retiolum-privateKey-rsa.age;
      mode = "400";
      owner = "tinc.retiolum";
      group = "tinc.retiolum";
    };
    retiolum-ed25519 = {
      file = ../../secrets/fatteh-retiolum-privateKey-ed25519.age;
      mode = "400";
      owner = "tinc.retiolum";
      group = "tinc.retiolum";
    };
    restic.file = ../../secrets/restic.age;
    syncthing-cert.file = ../../secrets/fatteh-syncthing-cert.age;
    syncthing-key.file = ../../secrets/fatteh-syncthing-key.age;
  };

  networking.hostName = "fatteh";
  networking.retiolum = retiolumAddresses.fatteh;

  system.stateVersion = "23.11";
}
