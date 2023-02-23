{
  lib,
  config,
  pkgs,
  ...
}: let
  inherit (import ../../lib) kieran retiolumAddresses restic;
in {
  imports = [
    ./gitea.nix
    ./hardware-configuration.nix
    ./hedgedoc.nix
    ./menstruation.nix
    ./moinbot.nix
    ./monitoring
    ./names.nix
    ./nextcloud.nix
    ./radio-news.nix
    ./radio.nix
    ./retiolum-map.nix
    ./tarot.nix
    ./tt-rss.nix
    ./urlwatch.nix
    ./weechat.nix
    ../../configs/monitoring.nix
    ../../configs/nix.nix
    ../../configs/save-space.nix
    ../../configs/retiolum.nix
    ../../configs/spacetime.nix
    ../../configs/sshd.nix
    ../../configs/telegram-bots
    ../../modules/passport.nix
  ];

  services.restic.backups.niveum = {
    initialize = true;
    inherit (restic) repository;
    timerConfig = {
      OnCalendar = "daily";
      RandomizedDelaySec = "1h";
    };
    passwordFile = config.age.secrets.restic.path;
    paths = [
      config.services.postgresqlBackup.location
      config.services.nextcloud.home
      config.services.grafana.dataDir
      config.services.gitea.stateDir
      config.services.weechat.root
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
    firewall.allowedTCPPorts = [80 443];
    hostName = "makanek";
    interfaces.ens3.useDHCP = true;
    retiolum = retiolumAddresses.makanek;
    useDHCP = false;
  };

  age.secrets = {
    retiolum-rsa.file = ../../secrets/makanek-retiolum-privateKey-rsa.age;
    retiolum-ed25519.file = ../../secrets/makanek-retiolum-privateKey-ed25519.age;
    restic.file = ../../secrets/restic.age;
  };

  system.stateVersion = "20.03";

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

  environment.systemPackages = [
    pkgs.vim
    pkgs.git
    pkgs.tmux
    pkgs.python3
    pkgs.nix-output-monitor
  ];
}
