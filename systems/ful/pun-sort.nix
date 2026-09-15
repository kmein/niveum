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
    wantedBy = [ "multi-user.target" ];
    environment = {
      PORT = toString punPort;
      PATH = lib.mkForce (lib.makeBinPath [ pkgs.espeak-ng ]);
    };
    serviceConfig = pkgs.lib.niveum.hardening // {
      Type = "simple";
      DynamicUser = true;
      ExecStart = lib.getExe pkgs.pun-sort-api;
    };
  };

  services.nginx.virtualHosts."pun-sort.${pkgs.lib.niveum.domain}" = {
    enableACME = true;
    forceSSL = true;
    locations = {
      "/".proxyPass = "http://127.0.0.1:${toString punPort}/";
    };
  };
}
