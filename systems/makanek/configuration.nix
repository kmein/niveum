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
    # ./menstruation.nix
    ./moinbot.nix
    ./monitoring
    # ./names.nix
    ./nextcloud.nix
    ./radio-news.nix
    ./scrabble.nix
    # ./onlyoffice.nix
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

  services.restic.backups.niveum = {
    paths = [
      config.services.postgresqlBackup.location
      config.services.nextcloud.home
      config.services.grafana.dataDir
      config.services.gitea.stateDir
      config.services.weechat.root
      config.services.nginx.virtualHosts."www.${domain}".locations."/".root
      "/var/lib/weechat"
      "/var/lib/codimd"
    ];
  };

  niveum.passport = {
    enable = true;
    introductionHTML = ''
      <p>
      The machine <tt>makanek</tt> is named after a Levantine type of <a href="https://en.wikipedia.org/wiki/Makanek">sausage</a> (مقانق <i>maqāniq</i>).
      </p>
      <p>
      It runs on <a href="https://www.hetzner.com/cloud">Hetzner cloud</a>.
      </p>
      <figure>
        <img width="200" src="https://www.albawaba.com/sites/default/files/2019-08/makanek-BeFunky-project.jpg" alt="Makanek sausages"/>
        <figcaption>Makanek</figcaption>
      </figure>
    '';
    virtualHost = "makanek.r";

    services = [
      {
        title = "restic backup";
        description = "This machine backups its state via restic backup.";
      }
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
