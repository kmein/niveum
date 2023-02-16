{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (import <niveum/lib>) retiolumAddresses restic;
in {
  imports = [
    ./atuin.nix
    ./backup.nix
    ./gaslight.nix
    ./hardware-configuration.nix
    ./kiosk.nix
    ./moodle-dl-meinhark.nix
    ./pulseaudio.nix
    ./home-assistant.nix
    ./mpd.nix
    ./grocy.nix
    ./spotifyd.nix
    <niveum/configs/keyboard.nix>
    <niveum/configs/monitoring.nix>
    <niveum/configs/nix.nix>
    <niveum/configs/printing.nix>
    <niveum/configs/spacetime.nix>
    <niveum/configs/sshd.nix>
    # <niveum/configs/traadfri.nix>
    <niveum/configs/tmux.nix>
    <niveum/configs/retiolum.nix>
    <niveum/configs/wpa_supplicant.nix>
  ];

  services.restic.backups.moodle-dl = {
    initialize = true;
    inherit (restic) repository;
    timerConfig = {
      OnCalendar = "daily";
      RandomizedDelaySec = "1h";
    };
    passwordFile = toString <secrets/restic/password>;
    paths = [
      "/var/lib/moodle-dl"
      "/var/lib/containers/storage/volumes/home-assistant"
      config.services.postgresqlBackup.location
    ];
  };

  nix.nixPath = ["/var/src"];

  services.logind = {
    lidSwitch = "ignore";
    lidSwitchDocked = "ignore";
    lidSwitchExternalPower = "ignore";
  };

  services.illum.enable = true;

  environment.systemPackages = let
    worldradio = pkgs.callPackage <niveum/packages/worldradio.nix> {};
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
  systemd.services.systemd-networkd-wait-online.enable = false;

  networking = {
    hostName = "zaatar";
    wireless.interfaces = ["wlp2s0"];
    retiolum = retiolumAddresses.zaatar;
  };

  system.stateVersion = "20.09";
}
