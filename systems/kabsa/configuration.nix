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
    ../../configs/battery.nix
    ../../configs/default.nix
    ../../configs/networkmanager.nix # TODO how to get passwords into there?
  ];

  niveum = {
    batteryName = "BAT0";
    wirelessInterface = "wlp3s0";
    promptColours.success = "cyan";
  };

  nix.settings = {
    cores = 1;
    max-jobs = 2;
  };

  age.secrets = {
    retiolum-rsa.file = ../../secrets/kabsa-retiolum-privateKey-rsa.age;
    retiolum-ed25519.file = ../../secrets/kabsa-retiolum-privateKey-ed25519.age;
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
