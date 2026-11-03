{
  lib,
  config,
  pkgs,
  ...
}:
let
  inherit (pkgs.lib.niveum) domain;
in
{
  imports = [
    ./gitea.nix
    ./hardware-configuration.nix
    ./hedgedoc.nix
    ./moinbot.nix
    ./monitoring
    ./radio-news.nix
    ./scrabble.nix
    ./retiolum-map.nix
    ./oracle
    ./tt-rss.nix
    ./weechat.nix
    ../../configs/nginx.nix
    ../../configs/restic-client.nix
    ../../configs/server-packages.nix
    ../../configs/tor.nix
    ../../configs/bots
  ];

  # not the website: it is on the storagebox already, next to this repository,
  # and the server repository must not hold personal files
  services.restic.backups.niveum = {
    paths = [
      config.services.postgresqlBackup.location
      config.services.gitea.stateDir
      config.services.weechat.root
      "/var/lib/weechat"
      "/var/lib/codimd"
    ];
  };

  networking = {
    firewall.allowedTCPPorts = [
      80
      443
    ];
    hostName = "makanek";
    interfaces.ens3.useDHCP = true;
    retiolum = pkgs.lib.niveum.retiolumAddresses.makanek;
    useDHCP = false;
  };

  age.secrets = {
    www-storagebox-credentials.file = ../../secrets/www-storagebox-credentials.age;
  };

  # the website lives on its own storagebox sub-account; uploads go through this
  # mount, its source is on fatteh
  fileSystems."/var/www/${domain}" = {
    device = "//u359050-sub5.your-storagebox.de/u359050-sub5";
    fsType = "cifs";
    options = [
      "iocharset=utf8"
      "rw"
      "seal"
      "credentials=${config.age.secrets.www-storagebox-credentials.path}"
      # cifs has no owners of its own: root uploads, nginx reads
      "uid=0"
      "gid=${config.services.nginx.group}"
      "file_mode=0640"
      "dir_mode=0750"
      # an unreachable storagebox must not keep makanek from booting
      "nofail"
    ];
  };

  system.stateVersion = "20.03";

  services.nginx.virtualHosts."www.${domain}" = {
    addSSL = true;
    enableACME = true;
    locations."/" = {
      root = "/var/www/${domain}";
      extraConfig = ''
        add_header 'Access-Control-Allow-Origin' '*';
        add_header 'Access-Control-Allow-Methods' 'GET, POST, OPTIONS';
        add_header 'Access-Control-Allow-Headers' 'Content-Type, Authorization';

        # Handle preflight requests
        if ($request_method = 'OPTIONS') {
            add_header 'Access-Control-Allow-Origin' '*';
            add_header 'Access-Control-Allow-Methods' 'GET, POST, OPTIONS';
            add_header 'Access-Control-Allow-Headers' 'Content-Type, Authorization';
            return 204; # No Content
        }
      '';
    };
  };

  environment.systemPackages = [
    pkgs.nix-output-monitor
  ];
}
