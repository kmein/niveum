{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (import ../../lib) retiolumAddresses;
in {
  imports = [
    ./hardware-configuration.nix
    ../../configs/tlp.nix
    ../../configs/default.nix
    ../../configs/networkmanager.nix
  ];

  niveum = {
    batteryName = "BAT0";
    wirelessInterface = "wlp3s0";
    promptColours.success = "cyan";
  };

  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/dracula.yaml";

  nix.settings = {
    cores = 1;
    max-jobs = 2;
  };

  age.secrets = {
    retiolum-rsa = {
      file = ../../secrets/kabsa-retiolum-privateKey-rsa.age;
      mode = "400";
      owner = "tinc.retiolum";
      group = "tinc.retiolum";
    };
    retiolum-ed25519 = {
      file = ../../secrets/kabsa-retiolum-privateKey-ed25519.age;
      mode = "400";
      owner = "tinc.retiolum";
      group = "tinc.retiolum";
    };
    restic.file = ../../secrets/restic.age;
    syncthing-cert.file = ../../secrets/kabsa-syncthing-cert.age;
    syncthing-key.file = ../../secrets/kabsa-syncthing-key.age;
  };

  environment.systemPackages = [pkgs.minecraft pkgs.zeroad];

  networking = {
    hostName = "kabsa";
    wireless.interfaces = ["wlp3s0"];
    retiolum = retiolumAddresses.kabsa;
  };

  system.stateVersion = "19.03";
}
