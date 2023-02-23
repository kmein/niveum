{
  pkgs,
  lib,
  inputs,
  ...
}: let
  port = 5703;
in {
  systemd.services.names = {
    wants = ["network-online.target"];
    wantedBy = ["multi-user.target"];
    description = "Better clone of geogen.stoepel.net";
    serviceConfig = {
      DynamicUser = true;
      ExecStart = "${inputs.scripts.packages.x86_64-linux.onomap}/bin/onomap-web";
      Restart = "on-failure";
      RestartSec = "15s";
    };
    environment.PORT = toString port;
  };

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
  };

  niveum.passport.services = [
    {
      link = "http://names.kmein.r";
      title = "Onomap";
      description = "maps surnames within Germany.";
    }
  ];

  services.nginx.virtualHosts."names.kmein.r" = {
    locations."/".proxyPass = "http://127.0.0.1:${toString port}";
  };
}
