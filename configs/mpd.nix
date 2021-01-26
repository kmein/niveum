{ config, pkgs, lib, ... }:
let
  streams = import <niveum/lib/streams.nix> {
    di-fm-key = lib.strings.fileContents <secrets/di.fm/key>;
  };
in
{
  imports = [ <niveum/modules/mpd-fm.nix> ];

  environment.systemPackages = [ pkgs.ncmpcpp pkgs.mpc_cli ];

  services.mpd-fm = {
    enable = true;
    stationsFile = "/etc/mpd-fm/stations.json";
    webPort = 8080;
  };

  systemd.services.antenne-asb =
  let
    stations = lib.lists.imap0 (id: {desc ? "", logo ? "https://picsum.photos/seed/${builtins.hashString "md5" stream}/300", stream, station}: { inherit id desc logo stream station; }) streams;
    stationsJson = pkgs.writeText "stations.json" (builtins.toJSON stations);
  in {
    wantedBy = [ "mpd-fm.service" ];
    startAt = "hourly";
    script = ''
      mkdir -p /etc/mpd-fm
      antenne_asb_url=$(
        ${pkgs.curl}/bin/curl -sS 'https://www.caster.fm/widgets/em_player.php?jsinit=true&uid=529295&t=blue&c=' \
          | grep streamUrl \
          | sed ${lib.escapeShellArg "s/^.*'\\([^']*\\)'.*/\\1/"}
      )
      ${pkgs.jq}/bin/jq "map(if .station == \"Antenne ASB\" then .stream |= \"$antenne_asb_url\" else . end)" < ${stationsJson} > /etc/mpd-fm/stations.json
    '';
  };

  services.mpd.enable = true;

  services.nginx = {
    upstreams."mpd-fm-socket" = {
      extraConfig = ''
        server 127.0.0.1:${toString config.services.mpd-fm.webPort};
      '';
    };
    appendHttpConfig = ''
      map $http_upgrade $connection_upgrade {
        default upgrade;
        ''' close;
      }
    '';
    virtualHosts.default = {
      basicAuth.dj = lib.strings.fileContents <system-secrets/mpd-web.key>;
      locations."/" = {
        proxyPass = "http://mpd-fm-socket";
        extraConfig = ''
          proxy_http_version 1.1;
          proxy_set_header Upgrade $http_upgrade;
          proxy_set_header Connection "Upgrade";
          proxy_set_header Host $host;
        ''; # generate password hash with `openssl passwd -apr1`
      };
    };
  };

  /*
  # dont let anyone outside localhost or local network in
  networking.firewall.extraCommands =
  let
    mpd-fm-port = toString config.services.mpd-fm.webPort;
  in ''
    ${pkgs.iptables}/bin/iptables -A INPUT -p tcp --dport ${mpd-fm-port} -s 192.168.0.0/16 -j ACCEPT
    ${pkgs.iptables}/bin/iptables -A INPUT -p tcp --dport ${mpd-fm-port} -s 10.243.2.4 -j ACCEPT
    ${pkgs.iptables}/bin/iptables -A INPUT -p tcp --dport ${mpd-fm-port} -s 127.0.0.0/8 -j ACCEPT
    ${pkgs.iptables}/bin/iptables -A INPUT -p tcp --dport ${mpd-fm-port} -j DROP
  '';
  */
}
