{
  config,
  pkgs,
  ...
}:
{
  imports = [
    ./cuda.nix
    ./applicative.nix
    ./hardware-configuration.nix
    ../../configs/networkmanager.nix
    ../../configs/default.nix
    ../../configs/ccc.nix
    ../../configs/gaming.nix
    # ../../configs/gnome.nix
  ];

  niveum = {
    batteryName = "BAT1";
    wirelessInterface = "wlp3s0";
    promptColours.success = "blue";
  };

  services.illum.enable = true;

  age.secrets = {
    retiolum-rsa = {
      file = ../../secrets/fatteh-retiolum-privateKey-rsa.age;
      mode = "400";
      owner = "tinc-retiolum";
      group = "tinc-retiolum";
    };
    retiolum-ed25519 = {
      file = ../../secrets/fatteh-retiolum-privateKey-ed25519.age;
      mode = "400";
      owner = "tinc-retiolum";
      group = "tinc-retiolum";
    };
    restic.file = ../../secrets/restic.age;
    syncthing-cert.file = ../../secrets/fatteh-syncthing-cert.age;
    syncthing-key.file = ../../secrets/fatteh-syncthing-key.age;
    wireguard-aether-key.file = ../../secrets/fatteh-wireguard-aether-key.age;
    wireguard-aether-psk.file = ../../secrets/fatteh-wireguard-aether-psk.age;
  };

  networking.hostName = "fatteh";
  networking.retiolum = pkgs.lib.niveum.retiolumAddresses.fatteh;

  system.stateVersion = "23.11";
}
