{
  config,
  pkgs,
  lib,
  ...
}:
let
  tarotPort = 7407;
  ichingPort = 1819;
  tarotFiles = pkgs.fetchzip {
    url = "https://c.krebsco.de/tarot.zip";
    sha256 = "0jl5vdwlj17pqp94yj02xgsb1gyvs9i08m83kac0jdnhfjl2f75a";
    stripRoot = false;
  };
  tarotKey = builtins.fetchurl {
    url = "http://c.krebsco.de/tarot.pdf";
    sha256 = "1n2m53kjg2vj9dbr70b9jrsbqwdfrcb48l4wswn21549fi24g6dx";
  };
in
{
  systemd.services.tarot = {
    enable = true;
    serviceConfig.Type = "simple";
    wantedBy = [ "multi-user.target" ];
    environment = {
      TAROT_FILES = tarotFiles;
      TAROT_PORT = toString tarotPort;
    };
    serviceConfig.ExecStart = pkgs.writers.writePython3 "tarot-server" {
      libraries = py: [
        py.pillow
        py.flask
      ];
    } ./tarot.py;
  };

  systemd.services.iching = {
    enable = true;
    serviceConfig.Type = "simple";
    wantedBy = [ "multi-user.target" ];
    environment = {
      ICHING_PORT = toString ichingPort;
    };
    serviceConfig.ExecStart = pkgs.writers.writePython3 "iching-server" {
      libraries = py: [
        py.flask
      ];
    } ./iching.py;
  };

  niveum.passport.services = [
    {
      link = "https://tarot.kmein.de";
      title = "Tarot";
      description = "draws Tarot cards for you.";
    }
    {
      link = "https://iching.kmein.de";
      title = "I Ching";
      description = "draws I Ching hexagrams for you.";
    }
  ];

  services.nginx.virtualHosts."iching.kmein.de" = {
    enableACME = true;
    forceSSL = true;
    locations = {
      "/".proxyPass = "http://127.0.0.1:${toString ichingPort}/";
    };
  };

  services.nginx.virtualHosts."tarot.kmein.de" = {
    enableACME = true;
    forceSSL = true;
    locations = {
      "/".proxyPass = "http://127.0.0.1:${toString tarotPort}/";
      "/files/" = {
        root = pkgs.linkFarm "tarot" [
          {
            name = "files/key.pdf";
            path = tarotKey;
          }
          {
            name = "files/cards";
            path = tarotFiles;
          }
        ];
        extraConfig = ''
          autoindex on;
          charset UTF-8;
        '';
      };
    };
  };
}
