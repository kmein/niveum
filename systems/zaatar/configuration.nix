{
  config,
  pkgs,
  lib,
  ...
}:
{
  imports = [
    ./backup.nix
    ./gaslight.nix
    ./hardware-configuration.nix
    ./home-assistant.nix
    ./music-assistant.nix
    ../../configs/printing.nix
    ../../configs/oci-containers.nix
    ../../configs/restic-client.nix
    ../../configs/wpa_supplicant.nix
  ];

  services.pipewire.systemWide = true;

  age.secrets = {
    wifi = {
      file = ../../secrets/wifi.age;
      owner = "wpa_supplicant";
      group = "wpa_supplicant";
    };
    restic-offsite = {
      file = ../../secrets/restic-offsite.age;
    };
    zaatar-khall-restic-ssh = {
      file = ../../secrets/zaatar-khall-restic-ssh.age;
      owner = "restic";
      group = "restic";
      mode = "400";
    };
    restic = {
      mode = "400";
      owner = "restic";
      group = "restic";
    };
  };

  services.restic.backups.niveum = {
    paths = [
      "/var/lib/moodle-dl"
      "/var/lib/containers/storage/volumes/home-assistant"
      config.services.postgresqlBackup.location
    ];
  };

  services.logind.settings.Login.HandleLidSwitchDocked = "ignore";
  services.logind.settings.Login.HandleLidSwitchExternalPower = "ignore";
  services.logind.settings.Login.HandleLidSwitch = "ignore";

  services.illum.enable = true;

  environment.systemPackages =
    let
      worldradio = pkgs.callPackage ../../packages/worldradio.nix { };
    in
    [
      (pkgs.writers.writeDashBin "mpv" ''${pkgs.mpv}/bin/mpv --no-video "$@"'')
      (pkgs.writers.writeDashBin "worldradio" ''
        shuf ${worldradio} | ${pkgs.findutils}/bin/xargs ${pkgs.mpv}/bin/mpv --no-video
      '')
      pkgs.git
      pkgs.vim
      pkgs.htop
      pkgs.ncmpcpp
      pkgs.python3 # for sshuttle
    ];

  users.users.root.extraGroups = [
    "lp"
    "scanner"
  ];

  networking = {
    hostName = "zaatar";
    wireless.interfaces = [ "wlp2s0" ];
    retiolum = pkgs.lib.niveum.retiolumAddresses.zaatar;
  };

  system.stateVersion = "23.11";
}
