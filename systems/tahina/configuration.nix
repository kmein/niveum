{
  config,
  pkgs,
  ...
}: let
  inherit (import ../../lib) retiolumAddresses restic;
in {
  imports = [
    ./hardware-configuration.nix
    ./home-assistant.nix
    ./backup.nix
    ./atuin.nix
    ../../configs/spacetime.nix
    ../../configs/sshd.nix
    ../../configs/printing.nix
    ../../configs/monitoring.nix
    ../../configs/tmux.nix
    ../../configs/retiolum.nix
    ../../configs/nix.nix
    ../../configs/admin-essentials.nix
    ../../configs/wpa_supplicant.nix
  ];

  age.secrets = {
    retiolum-rsa = {
      file = ../../secrets/tahina-retiolum-privateKey-rsa.age;
      mode = "400";
      owner = "tinc-retiolum";
      group = "tinc-retiolum";
    };
    retiolum-ed25519 = {
      file = ../../secrets/tahina-retiolum-privateKey-ed25519.age;
      mode = "400";
      owner = "tinc-retiolum";
      group = "tinc-retiolum";
    };
    restic = {
      file = ../../secrets/restic.age;
      mode = "400";
      owner = "restic";
      group = "restic";
    };
  };

  services.restic.backups.niveum = {
    initialize = true;
    inherit (restic) repository;
    timerConfig = {
      OnCalendar = "daily";
      RandomizedDelaySec = "1h";
    };
    passwordFile = config.age.secrets.restic.path;
    paths = [
      "/var/lib/moodle-dl"
      "/var/lib/containers/storage/volumes/home-assistant"
      config.services.postgresqlBackup.location
    ];
  };

  services.logind = {
    lidSwitch = "ignore";
    lidSwitchDocked = "ignore";
    lidSwitchExternalPower = "ignore";
    suspendKey = "ignore";
    suspendKeyLongPress = "ignore";
    hibernateKey = "ignore";
    hibernateKeyLongPress = "ignore";
  };

  services.illum.enable = true;

  networking = {
    useDHCP = false;
    interfaces = {
      enp0s25.useDHCP = true;
      wlo1.useDHCP = true;
    };
    retiolum = retiolumAddresses.tahina;
    hostName = "tahina";
  };

  system.stateVersion = "21.11";
}
