{
  config,
  pkgs,
  lib,
  ...
}:
let
  punPort = 9007;
in
{
  systemd.services.pun-sort = {
    enable = true;
    serviceConfig.Type = "simple";
    wantedBy = [ "multi-user.target" ];
    environment = {
      PORT = toString punPort;
      PATH = lib.mkForce (lib.makeBinPath [ pkgs.espeak-ng ]);
    };
    serviceConfig.ExecStart = lib.getExe pkgs.pun-sort-api;
  };

  services.nginx.virtualHosts."pun-sort.kmein.de" = {
    enableACME = true;
    forceSSL = true;
    locations = {
      "/".proxyPass = "http://127.0.0.1:${toString punPort}/";
    };
  };
}
