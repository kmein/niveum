{
  config,
  pkgs,
  lib,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
    ./matomo.nix
    ./pun-sort.nix
    ./radio.nix
    ./panoptikon.nix
    ./pr-notifier.nix
    ./hledger.nix
    ./go-webring.nix
    ./gemini.nix
    ./wallabag.nix
    ./nethack.nix
    ./opencrow.nix
    ./meteora.nix
    ../../configs/oci-containers.nix
    ../../configs/restic-client.nix
    ../../configs/server-packages.nix
    ../../configs/nginx.nix
  ];

  niveum.passport = {
    enable = true;
    introductionHTML = "";
    virtualHost = "ful.r";

    services = [
      {
        title = "restic backup";
        description = "This machine backups its state via restic backup.";
      }
    ];
  };

  age.secrets = {
    root.file = ../../secrets/ful-root.age;
    pr-notifier-smtp.file = ../../secrets/pr-notifier-smtp.age;
    pr-notifier-github.file = ../../secrets/pr-notifier-github.age;
  };

  services.restic.backups.niveum = {
    paths = [
      config.services.mysqlBackup.location
    ];
  };

  networking = {
    firewall.allowedTCPPorts = [
      80
      443
    ];
    hostName = "ful";
    interfaces.enp0s3.useDHCP = true;
    retiolum = pkgs.lib.niveum.retiolumAddresses.ful;
    useDHCP = false;
  };

  system.stateVersion = "21.11";

  users.users.root.hashedPasswordFile = config.age.secrets.root.path;

  # since 22.05 timeout fails?
  # systemd.services.systemd-networkd-wait-online.enable = false;
}
