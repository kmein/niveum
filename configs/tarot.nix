{ config, pkgs, lib, ... }:
let
  tarotPort = 7407;
  tarotFiles = pkgs.fetchzip {
    url = "https://c.krebsco.de/tarot.zip";
    sha256 = "0jl5vdwlj17pqp94yj02xgsb1gyvs9i08m83kac0jdnhfjl2f75a";
    stripRoot = false;
  };
in
{
  krebs.htgen.tarot = {
    port = tarotPort;
    user.name = "radio";
    script = ''. ${pkgs.writers.writeDash "tarot" ''
      case "$Method $Request_URI" in
        "GET /")
          if item=$(find ${toString tarotFiles} -type f | shuf -n1); then
            printf 'HTTP/1.1 200 OK\r\n'
            printf 'Content-Type: %s\r\n' "$(file -ib $item)"
            printf 'Server: %s\r\n' "$Server"
            printf 'Connection: close\r\n'
            printf 'Content-Length: %d\r\n' $(wc -c < $item)
            printf '\r\n'
            cat $item
            exit
          fi
        ;;
      esac
    ''}'';
  };

  services.nginx.virtualHosts."tarot.kmein.de" = {
    enableACME = true;
    forceSSL = true;
    locations."/".proxyPass = "http://127.0.0.1:${toString tarotPort}";
  };
}
