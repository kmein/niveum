{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (import ../../lib) kieran retiolumAddresses restic;
in {
  imports = [
    ./hardware-configuration.nix
    ./matomo.nix
    ./radio.nix
    ./panoptikon.nix
    ./hledger.nix
    ./alew.nix
    ../../configs/monitoring.nix
    ../../configs/tor.nix
    ../../configs/save-space.nix
    ../../configs/spacetime.nix
    ../../configs/retiolum.nix
    ../../configs/sshd.nix
    ../../configs/nix.nix
    ../../configs/admin-essentials.nix
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
      owner = "tinc-retiolum";
      group = "tinc-retiolum";
    };
    retiolum-ed25519 = {
      file = ../../secrets/ful-retiolum-privateKey-ed25519.age;
      mode = "400";
      owner = "tinc-retiolum";
      group = "tinc-retiolum";
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


  users.users.servant = {
    isSystemUser = true;
    group = "servant";
  };
  users.groups.servant = {};
  systemd.services.servant = {
    enable = true;
    environment.PORT = toString 18987;
    environment.VIRTUAL_HOST = "https://openapiaiapi.kmein.de";
    serviceConfig.ExecStart = pkgs.writers.writeHaskell "server" {
      libraries = with pkgs.haskellPackages; [
        servant
        servant-server
        servant-openapi3
        servant-swagger-ui
        servant-client
        aeson
        text
        warp
        uuid
        lens
      ];
      ghcArgs = ["-O3" "-threaded"];
    } ./servant-openapi.hs;
    serviceConfig.User = "servant";
    serviceConfig.Group = "servant";
  };

  services.htgen.openapi-conversion = {
    port = 18988;
    script = ''. ${pkgs.writers.writeDash "openapi-conversion" ''
      case "$Method $Request_URI" in
        "GET /openapi-3.1.json")
          schema=$(mktemp -d)
          trap 'rm -rf $schema' EXIT
          ${pkgs.wget}/bin/wget http://127.0.0.1:${toString 18987}/openapi.json -O "$schema"/openapi.json
          cat  "$schema"/openapi.json >&2
          PATH=${lib.makeBinPath [pkgs.bashInteractive pkgs.nodejs]} ${pkgs.nodejs}/bin/npx --yes openapi-format "$schema"/openapi.json --convertTo "3.1" -o "$schema"/openapi-new.json
          printf 'HTTP/1.1 200 OK\r\n'
          printf 'Content-Type: %s\r\n' "$(${pkgs.file}/bin/file -ib "$schema"/openapi-new.json)"
          printf 'Server: %s\r\n' "$Server"
          printf 'Connection: close\r\n'
          printf 'Content-Length: %d\r\n' $(${pkgs.coreutils}/bin/wc -c < "$schema"/openapi-new.json)
          printf '\r\n'
          cat "$schema"/openapi-new.json
          exit
        ;;
      esac
    ''}'';
  };

  services.nginx.virtualHosts."openapiaiapi.kmein.de" = {
    enableACME = true;
    forceSSL = true;
    locations."/openapi-3.1.json".proxyPass = "http://127.0.0.1:${toString 18988}";
    locations."/".proxyPass = "http://127.0.0.1:${toString 18987}";
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

  users.users.root.hashedPasswordFile = config.age.secrets.root.path;

  environment.systemPackages = [pkgs.vim pkgs.git pkgs.tmux pkgs.python3];

  # since 22.05 timeout fails?
  # systemd.services.systemd-networkd-wait-online.enable = false;
}
