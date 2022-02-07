{ config, pkgs, lib, ... }:
let
  tarotPort = 7407;
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
  imports = [ <stockholm/krebs/3modules/htgen.nix> ];

  krebs.htgen.tarot = {
    port = tarotPort;
    user.name = "radio";
    script = ''. ${pkgs.writers.writeDash "tarot" ''
      case "$Method $Request_URI" in
        "GET /")
          if item=$(${pkgs.findutils}/bin/find ${toString tarotFiles} -type f | ${pkgs.coreutils}/bin/shuf -n1); then
            card=$(mktemp --tmpdir tarot.XXX)
            trap 'rm $card' EXIT
            reverse=$(${pkgs.coreutils}/bin/shuf -i0-1 -n1)
            if [ "$reverse" -eq 1 ]; then
              ${pkgs.imagemagick}/bin/convert -rotate 180 "$item" "$card"
            else
              ${pkgs.coreutils}/bin/cp "$item" "$card"
            fi
            printf 'HTTP/1.1 200 OK\r\n'
            printf 'Content-Type: %s\r\n' "$(${pkgs.file}/bin/file -ib "$card")"
            printf 'Server: %s\r\n' "$Server"
            printf 'Connection: close\r\n'
            printf 'Content-Length: %d\r\n' $(${pkgs.coreutils}/bin/wc -c < "$card")
            printf '\r\n'
            cat "$card"
            exit
          fi
        ;;
      esac
    ''}'';
  };

  services.nginx.virtualHosts."tarot.kmein.de" = {
    enableACME = true;
    forceSSL = true;
    locations = {
      "/".proxyPass = "http://127.0.0.1:${toString tarotPort}";
      "/files/" = {
        root = pkgs.linkFarm "tarot" [
          { name = "files/key.pdf"; path = tarotKey; }
          { name = "files/cards"; path = tarotFiles; }
        ];
        extraConfig = ''
          autoindex on;
          charset UTF-8;
        '';
      };
    };
  };
}
