{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (import ../../lib) retiolumAddresses restic;
in {
  imports = [
    ./atuin.nix
    ./backup.nix
    ./gaslight.nix
    ./kiosk.nix
    ./hardware-configuration.nix
    ./moodle-dl-meinhark.nix
    ./pulseaudio.nix
    ./home-assistant.nix
    ./mpd.nix
    ./grocy.nix
    ./spotifyd.nix
    ../../configs/keyboard.nix
    ../../configs/monitoring.nix
    ../../configs/retiolum.nix
    ../../configs/printing.nix
    ../../configs/spacetime.nix
    ../../configs/sshd.nix
    ../../configs/tmux.nix
    ../../configs/wpa_supplicant.nix
    ../../configs/nix.nix
  ];

  age.secrets = {
    retiolum-rsa = {
      file = ../../secrets/zaatar-retiolum-privateKey-rsa.age;
      mode = "400";
      owner = "tinc.retiolum";
      group = "tinc.retiolum";
    };
    retiolum-ed25519 = {
      file = ../../secrets/zaatar-retiolum-privateKey-ed25519.age;
      mode = "400";
      owner = "tinc.retiolum";
      group = "tinc.retiolum";
    };
    restic.file = ../../secrets/restic.age;
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
  };

  services.illum.enable = true;

  environment.systemPackages = let
    worldradio = pkgs.callPackage ../../packages/worldradio.nix {};
  in [
    (pkgs.writers.writeDashBin "mpv" ''${pkgs.mpv}/bin/mpv --no-video "$@"'')
    (pkgs.writers.writeDashBin "worldradio" ''
      shuf ${worldradio} | ${pkgs.findutils}/bin/xargs ${pkgs.mpv}/bin/mpv --no-video
    '')
    pkgs.git
    pkgs.vim
    pkgs.htop
    pkgs.ncmpcpp
  ];

  # since 22.05 timeout fails?
  # systemd.services.systemd-networkd-wait-online.enable = false;

  networking = {
    hostName = "zaatar";
    wireless.interfaces = ["wlp2s0"];
    retiolum = retiolumAddresses.zaatar;
  };

  system.stateVersion = "22.05";
}
