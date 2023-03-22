{
  lib,
  config,
  pkgs,
  ...
}: let
  inherit (import ../../lib) kieran retiolumAddresses restic;
in {
  imports = [
    ./hardware-configuration.nix
    ./matomo.nix
    ./radio.nix
    ../../configs/monitoring.nix
    ../../configs/tor.nix
    ../../configs/save-space.nix
    ../../configs/spacetime.nix
    ../../configs/retiolum.nix
    ../../configs/sshd.nix
    ../../configs/nix.nix
  ];

  niveum.passport = {
    enable = true;
    introductionHTML = ''
    '';
    virtualHost = "ful.r";

    services = [
      {
        title = "restic backup";
        description = "This machine backups its state via restic backup.";
      }
    ];
  };

  age.secrets = {
    retiolum-rsa = {
      file = ../../secrets/ful-retiolum-privateKey-rsa.age;
      mode = "400";
      owner = "tinc.retiolum";
      group = "tinc.retiolum";
    };
    retiolum-ed25519 = {
      file = ../../secrets/ful-retiolum-privateKey-ed25519.age;
      mode = "400";
      owner = "tinc.retiolum";
      group = "tinc.retiolum";
    };
    root.file = ../../secrets/ful-root.age;
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
      config.services.mysqlBackup.location
    ];
  };

  networking = {
    firewall.allowedTCPPorts = [80 443];
    hostName = "ful";
    interfaces.enp0s3.useDHCP = true;
    retiolum = retiolumAddresses.ful;
    useDHCP = false;
  };

  system.stateVersion = "21.11";

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    sslCiphers = "AES256+EECDH:AES256+EDH:!aNULL";
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = kieran.email;
  };

  users.users.root.passwordFile = config.age.secrets.root.path;

  environment.systemPackages = [pkgs.vim pkgs.git pkgs.tmux pkgs.python3];

  # since 22.05 timeout fails?
  systemd.services.systemd-networkd-wait-online.enable = false;
}
