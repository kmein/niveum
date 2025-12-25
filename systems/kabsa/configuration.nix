{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ../kibbeh/hardware-configuration.nix
    ../../configs/tlp.nix
    ../../configs/default.nix
    ../../configs/networkmanager.nix
    ../../configs/power-action.nix
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
    retiolum-rsa = {
      file = ../../secrets/kabsa-retiolum-privateKey-rsa.age;
      mode = "400";
      owner = "tinc-retiolum";
      group = "tinc-retiolum";
    };
    retiolum-ed25519 = {
      file = ../../secrets/kabsa-retiolum-privateKey-ed25519.age;
      mode = "400";
      owner = "tinc-retiolum";
      group = "tinc-retiolum";
    };
    restic.file = ../../secrets/restic.age;
    syncthing-cert.file = ../../secrets/kabsa-syncthing-cert.age;
    syncthing-key.file = ../../secrets/kabsa-syncthing-key.age;
    wireguard-aether-key.file = ../../secrets/kabsa-wireguard-aether-key.age;
    wireguard-aether-psk.file = ../../secrets/kabsa-wireguard-aether-psk.age;
  };

  environment.systemPackages = [pkgs.zeroad];

  networking = {
    hostName = "kabsa";
    wireless.interfaces = ["wlp3s0"];
    retiolum = pkgs.lib.niveum.retiolumAddresses.kabsa;
  };

  system.stateVersion = "23.11";
}
